::@echo off
setlocal enabledelayedexpansion enableextensions
set pasdoc=
for %%x in (*.pas) do set pasdoc=!pasdoc! %%x
D:\lazarus\tools\pasdoc\pasdoc.exe --output=_doc --title="Estate parser" --use-tipue-search --ignore-leading=* --write-uses-list --language ru.utf8 %pasdoc%
