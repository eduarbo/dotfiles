path=( "/Applications/Docker.app/Contents/Resources/bin/" $path )

export ANDROID_HOME=$HOME/Library/Android/sdk
path=( "$ANDROID_HOME/platform-tools" "$ANDROID_HOME/emulator" $path)
