for %%F in (apps/*.apk) do (
  adb install apps/%%F
)
pause