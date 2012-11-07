package uk.ac.ucl.excites.compression;


// // Copyright (c) 2010 Kevin Coulombe
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;


public class CharacterTree {
    
    private class Node implements Comparable<Node> {
        private final Integer letter;
        private final int frequence;
        private final Node lowerNode;
        private final Node higherNode;
        
        public Node(Integer letter, int frequence) {
            this.lowerNode = null;
            this.higherNode = null;
            this.letter = letter;
            this.frequence = frequence;
        }
        
        public Node(Node lowerNode, Node higherNode) {
            this.lowerNode = lowerNode;
            this.higherNode = higherNode;
            this.letter = null;
            this.frequence = lowerNode.frequence + higherNode.frequence;
        }

        public Node(LinkedList<Integer> letterQueue, BitInputStream inputStream) throws IOException {
            if (inputStream.readBit()) {
                letter = letterQueue.pop();
                frequence = 0; // unknown
                lowerNode = null;
                higherNode = null;
            }
            else {
                letter = null;
                frequence = 0; // unknown
                lowerNode = new Node(letterQueue, inputStream);
                higherNode = new Node(letterQueue, inputStream);
            }
        }

        /**
         * @return the letter
         */
        public int getLetter() {
            return letter;
        }

        /**
         * @return the frequence
         */
        public int getFrequence() {
            return frequence;
        }

        /**
         * @return the lowerNode
         */
        public Node getLowerNode() {
            return lowerNode;
        }

        /**
         * @return the higherNode
         */
        public Node getHigherNode() {
            return higherNode;
        }

        public int compareTo(Node comparee) {
            return this.frequence - comparee.frequence;
        }
        
        public boolean isLeaf() {
            return (letter != null);
        }

        public ByteArrayOutputStream read(BitInputStream compressedInputStream, ByteArrayOutputStream decompressedOutputStream) throws IOException {
            if (this.isLeaf()) {
                decompressedOutputStream.write(this.letter);
            }
            else {
                try {
                    if (compressedInputStream.readBit()) {
                        lowerNode.read(compressedInputStream, decompressedOutputStream);
                    }
                    else {
                        higherNode.read(compressedInputStream, decompressedOutputStream);
                    }
                }
                catch (EOFException ex) {
                    throw ex;
                }
            }
			return decompressedOutputStream;
        }

        public void fillPathMap(Map<Integer, Boolean[]> pathMap) {
            fillPathMap(pathMap, new Boolean[0]);
        }
        
        private void fillPathMap(Map<Integer, Boolean[]> pathMap, Boolean[] prefix) {
            if (this.isLeaf()) {
                pathMap.put(this.letter, prefix);
            }
            else {
                Boolean[] lowerAddress = Arrays.copyOf(prefix, prefix.length + 1);
                lowerAddress[lowerAddress.length - 1] = true;
                lowerNode.fillPathMap(pathMap, lowerAddress);
                
                Boolean[] higherAddress = Arrays.copyOf(prefix, prefix.length + 1);
                higherAddress[higherAddress.length - 1] = false;
                higherNode.fillPathMap(pathMap, higherAddress);
            }
        }

        public void writeLetters(BitOutputStream outputStream, ArrayList<Integer> letterMap) throws IOException {
            if (this.isLeaf()) {
                outputStream.writeBinary(letterMap.indexOf(this.letter), getLastSetBitIndex(letterMap.size() - 1));
                letterMap.remove(this.letter);
            }
            else {
                lowerNode.writeLetters(outputStream, letterMap);
                higherNode.writeLetters(outputStream, letterMap);
            }
        }

        public void writePaths(BitOutputStream outputStream) throws IOException {
            if (!this.isLeaf()) {
                outputStream.writeBit(false);
                lowerNode.writePaths(outputStream);
                outputStream.writeBit(true);
                higherNode.writePaths(outputStream);
            }
        }

        public int countLeaves() {
            if (this.isLeaf()) {
                return 1;
            }
            else {
                return lowerNode.countLeaves() + higherNode.countLeaves();
            }
        }
        
        @Override
        public String toString() {
            return toString(0);
        }
        
        private String toString(int level) {
            if (this.isLeaf()) {
                return level + " - " + this.letter + " - " + this.frequence;
            }
            else {
                String lowerToString = lowerNode.toString(level + 1);
                String higherToString = higherNode.toString(level + 1);
                
                return lowerToString + "\n" + higherToString;
            }
        }
        
        public int countDepth() {
            if (this.isLeaf()) {
                return 1;
            }
            else {
                int lowerDepth = lowerNode.countDepth();
                int higherDepth = higherNode.countDepth();
                
                if (lowerDepth > higherDepth) {
                    return lowerDepth + 1;
                }
                else {
                    return higherDepth + 1;
                }
            }
        }
    }
    
    private Node rootNode;
    private Map<Integer, Boolean[]> pathMap;
    private Map<Integer, Integer> frequenceTable;
    
    public CharacterTree(Map<Integer, Integer> frequenceTable) {
        initFromFrequenceTable(frequenceTable);
    }

    public CharacterTree(BitInputStream inputStream) throws IOException {
        int letterCount = inputStream.read() + 1;
        
        ArrayList<Integer> lettersMap = getLetterMap();
        LinkedList<Integer> letterQueue = new LinkedList<Integer>();
        
        for (int i = 0; i < letterCount; i++) {
            int letterIndex = inputStream.readBinary(getLastSetBitIndex(lettersMap.size() - 1));
            
            letterQueue.add(lettersMap.get(letterIndex));
            lettersMap.remove(letterIndex);
        }
        
        rootNode = new Node(letterQueue, inputStream);
    }

    private void initFromFrequenceTable(Map<Integer, Integer> frequenceTable) {
        this.frequenceTable = frequenceTable;
        
        if (frequenceTable.size() < 2) {
            throw new IllegalArgumentException("The frequence table must have at least two element.");
        }
        
        ArrayList<Node> nodeList = getNodeList(frequenceTable);
        Collections.sort(nodeList);
        
        while (nodeList.size() > 1) {
            nodeList.add(new Node(nodeList.remove(0), nodeList.remove(0)));
            Collections.sort(nodeList);
        }
        
        assert(nodeList.size() > 0);
        
        this.rootNode = nodeList.get(0);
        
        System.out.println(this.rootNode.toString());
        
        pathMap = new TreeMap<Integer, Boolean[]>(); 
        this.rootNode.fillPathMap(pathMap);
    }

    private ArrayList<Node> getNodeList(Map<Integer, Integer> frequenceTable) {
        ArrayList<Node> nodeList = new ArrayList<Node>();
        
        for (int key : frequenceTable.keySet()) {
            nodeList.add(new Node(key, frequenceTable.get(key)));
        }
        
        Collections.sort(nodeList);
        return nodeList;
    }

    public ByteArrayOutputStream read(BitInputStream compressedInputStream, ByteArrayOutputStream decompressedOutputStream) throws IOException {
        try {
            while (true) {
                rootNode.read(compressedInputStream, decompressedOutputStream);
            }
        }
        catch (EOFException ex) {
            // finished
        }
		return decompressedOutputStream;
    }
    
    public void write(InputStream uncompressedInputStream, BitOutputStream compressedOutputStream) throws IOException {
        int readByte;
        
        while ((readByte = uncompressedInputStream.read()) >= 0) {
            for (boolean value : pathMap.get(readByte)) {
                compressedOutputStream.writeBit(value);
            }
        }
    }

    public void serializeTo(BitOutputStream outputStream) throws IOException {
        int letterCount = rootNode.countLeaves();
        outputStream.write(letterCount - 1);
        
        ArrayList<Integer> lettersMap = getLetterMap();
        
        rootNode.writeLetters(outputStream, lettersMap);
        rootNode.writePaths(outputStream);
        outputStream.writeBit(true);
    }

    private ArrayList<Integer> getLetterMap() {
        ArrayList<Integer> lettersMap = new ArrayList<Integer>(256);
        
        for (int i = 0; i < 256; i++) {
            lettersMap.add(i);
        }
        return lettersMap;
    }
    

    private int getLastSetBitIndex(int value) {
        int bitIndex = 0;
        
        while (value > 0) {
            value >>= 1;
            bitIndex++;
        }
        
        return bitIndex;
    }
}
