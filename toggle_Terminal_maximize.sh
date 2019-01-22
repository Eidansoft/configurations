#!/usr/bin/osascript

tell application "Terminal" to activate

tell application "System Events"
    keystroke "f" using {command down, control down}
end tell
