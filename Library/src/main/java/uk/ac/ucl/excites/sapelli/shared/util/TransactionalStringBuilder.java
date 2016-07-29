/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.util;

import java.util.ArrayList;
import java.util.List;

/**
 * A transactional version of StringBuilder, supporting multiple nested transactions.
 * 
 * Each "transaction" behaves as a separate StringBuilder. There is always 1 "root" transaction,
 * which is opened as part of the construction of the TransactionalStringBuilder and which cannot
 * be closed (i.e. committed or rolled-back) from outside.
 * A new transaction can be opened using {@link #openTransaction()}, after which it becomes the current
 * transaction.
 * Calls to the append(String) method result in the current (i.e. most recently opened) transaction being
 * extended (i.e. the argument will be appended to the corresponding StringBuilder). 
 * A call to {@link #commitTransaction()} results in the current transaction, provided it was externally
 * opened, being closed with its resulting String being appended to the previous transaction (possibly
 * the root).
 * A call to {@link #rollbackTransaction()} or {@link #rollbackTransactions(int)} result in 1 or more
 * of the current, externally opened, transactions being discarded.
 * A call to {@link #toString()} returns the result of the root transaction, it can only be called when
 * any externally opened transactions have been closed (i.e. committed or rolled-back).
 *
 * The class supports automatic insertion of connectives between subsequent appendages. Each transaction can use a different connective.
 * 
 * @author mstevens
 */
public class TransactionalStringBuilder
{
	
	static public final String DEFAULT_CONNECTIVE = "";
	
	private final List<StringBuilder> builders;
	private final List<String> connectives;
	
	/**
	 * New TransactionalStringBuilder using the default (empty String) connective.
	 */
	public TransactionalStringBuilder()
	{
		this(DEFAULT_CONNECTIVE);
	}
	
	/**
	 * New TransactionalStringBuilder using the given connective.
	 * 
	 * @param baseConnective a char
	 */
	public TransactionalStringBuilder(char baseConnective)
	{
		this("" + baseConnective);
	}
	
	/**
	 * New TransactionalStringBuilder using the given connective.
	 * 
	 * @param baseConnective a String
	 */
	public TransactionalStringBuilder(String baseConnective)
	{
		builders = new ArrayList<StringBuilder>();
		connectives = new ArrayList<String>();
		openTransaction(baseConnective); // open root transaction
	}
	
	/**
	 * Open a new transaction using the 'base' connective specified at construction time.
	 */
	public void openTransaction()
	{
		openTransaction(connectives.get(0));
	}

	/**
	 * Open a new transaction in which to use the given connective
	 * 
	 * @param connective a char
	 */
	public void openTransaction(char connective)
	{
		openTransaction("" + connective);
	}
	
	/**
	 * Open a new transaction in which to use the given connective
	 * 
	 * @param connective a String
	 */
	public void openTransaction(String connective)
	{
		if(connective == null)
			throw new NullPointerException("Connectives cannot be null!");
		builders.add(new StringBuilder()); // new current transaction
		connectives.add(connective);
	}
	
	/**
	 * @return the number of open transactions
	 */
	public int numberOfOpenTransactions()
	{
		return builders.size() - 1; // only counting externally opened transactions (not the root transaction)
	}
	
	/**
	 * @return whether or not there is at least one open transaction
	 */
	public boolean hasOpenTransactions()
	{
		return builders.size() > 1; // only externally opened transactions count (not the root transaction) 
	}
	
	/**
	 * @return
	 */
	public boolean isCurrentTransactionEmpty()
	{
		return currentBuilder().length() == 0;
	}
	
	private StringBuilder currentBuilder()
	{
		return builders.get(builders.size() - 1);
	}
	
	private String currentConnective()
	{
		return connectives.get(connectives.size() - 1);
	}
	
	/**
	 * Discards the current transaction, unless it is the root transaction (i.e. not an externally opened one)
	 */
	private void discardTransaction()
	{
		if(hasOpenTransactions())
		{
			builders.remove(builders.size() - 1);
			connectives.remove(connectives.size() - 1);
		}
	}
	
	/**
	 * Rollback multiple transactions
	 * 
	 * @param numberOfTransactionsToRollback
	 * @throws IllegalArgumentException when there are not enough transactions to rollback
	 */
	public void rollbackTransactions(int numberOfTransactionsToRollback) throws IllegalArgumentException
	{
		if(numberOfTransactionsToRollback > numberOfOpenTransactions())
			throw new IllegalStateException("There are not enough transactions to rollback (current open transactions: " + numberOfOpenTransactions() + "; requested to rollback: " + numberOfTransactionsToRollback + ").");
		for(int t = 0; t < numberOfTransactionsToRollback; t++)
			discardTransaction();
	}
	
	/**
	 * Rollback the current transaction
	 * 
	 * @throws IllegalStateException when there is no transaction to rollback
	 */
	public void rollbackTransaction() throws IllegalStateException
	{
		if(!hasOpenTransactions())
			throw new IllegalStateException("There is no transaction to commit");
		discardTransaction();
	}
	
	/**
	 * Commit the current transaction.
	 * This means the String built during the current transaction is appended to previous transaction (possibly the root), as well as being returned.
	 * 
	 * @return String built during the committed transaction
	 * @throws IllegalStateException when there is no transaction to commit
	 */
	public String commitTransaction() throws IllegalStateException
	{
		return commitTransaction(true);
	}
	
	/**
	 * Commit the current transaction.
	 * This means the String built during the current transaction is appended to previous transaction (possibly the root), as well as being returned.
	 * 
	 * @param useConnective whether or not to insert the connective (if needed) when appending to previous transaction
	 * @return String built during the committed transaction
	 * @throws IllegalStateException when there is no transaction to commit
	 */
	public String commitTransaction(boolean useConnective) throws IllegalStateException
	{
		if(!hasOpenTransactions())
			throw new IllegalStateException("There is no transaction to commit.");
		return commitToString(useConnective);
	}
	
	/**
	 * Commit multiple transactions.
	 * 
	 * @param numberOfTransactionsToCommit
	 * @throws IllegalArgumentException when there are not enough transactions to commit
	 */
	public void commitTransactions(int numberOfTransactionsToCommit) throws IllegalArgumentException
	{
		commitTransactions(numberOfTransactionsToCommit, true);
	}

	/**
	 * Commit multiple transactions.
	 * 
	 * @param numberOfTransactionsToCommit
	 * @param useConnective whether or not to insert the connective (if needed) when appending to previous transaction
	 * @throws IllegalArgumentException when there are not enough transactions to commit
	 */
	public void commitTransactions(int numberOfTransactionsToCommit, boolean useConnective) throws IllegalArgumentException
	{
		if(numberOfTransactionsToCommit > numberOfOpenTransactions())
			throw new IllegalStateException("There are not enough transactions to commit (current open transactions: " + numberOfOpenTransactions() + "; requested to rollback: " + numberOfTransactionsToCommit + ").");
		for(int t = 0; t < numberOfTransactionsToCommit; t++)
			commitTransaction(useConnective);
	}
	
	/**
	 * Commits the current transaction. The resulting String is returns as well as appended to the parent transaction if there is one.
	 * 
	 * @return
	 */
	private String commitToString(boolean useConnective)
	{
		StringBuilder bldr = currentBuilder();
		String result = bldr.toString(); 
		discardTransaction(); // does nothing if we are at the root transaction
		if(currentBuilder() != bldr)
			append(result, useConnective); // append result to new current transaction unless it's the same one (meaning we are at the root)
		return result;
	}

	/**
	 * Append a String to the current transaction (possibly the root), connective will be used
	 * 
	 * @param str
	 * @return
	 */
	public TransactionalStringBuilder append(String str)
	{
		return append(str, true);
	}
	
	/**
	 * Append a char to the current transaction (possibly the root), connective will be used
	 * 
	 * @param chr
	 * @return
	 */
	public TransactionalStringBuilder append(char chr)
	{
		return append("" + chr, true);
	}
	
	/**
	 * Append a String to the current transaction (possibly the root)
	 * 
	 * @param str
	 * @param useConnective whether or not to insert the connective before the String (if needed)
	 * @return
	 */
	public TransactionalStringBuilder append(String str, boolean useConnective)
	{
		if(str != null && !str.isEmpty())
		{
			StringBuilder bldr = currentBuilder();
			String connective = currentConnective();
			if(useConnective && !connective.isEmpty() && bldr.length() > 0)
				bldr.append(connective);
			bldr.append(str);
		}
		return this;
	}
	
	/**
	 * Returns the total length of the String built across all committed(!) transactions 
	 * 
	 * @return
	 */
	public int length()
	{
		return builders.get(0).length();
	}
	
	/**
	 * @return whether or not the builder is still empty (note we only take committed transactions into account!)
	 */
	public boolean isEmpty()
	{
		return length() == 0;
	}
	
	/**
	 * @return the String built in the root transaction
	 * @throws IllegalStateException when there are still externally opened transaction that have not yet been committed/rolled-back
	 */
	@Override
	public String toString() throws IllegalStateException
	{			
		if(hasOpenTransactions())
			throw new IllegalStateException("There are " + numberOfOpenTransactions() + " open transactions!");
		return commitToString(false /* or true, it has not effect in this case */);
	}
	
}