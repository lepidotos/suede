#Initialize medusa-idled if this is an X login session

# We have a local X session, so start the medusa-idled

case "$DISPLAY" in
	":0"|":0.0"|"$HOSTNAME:0.0")
		/usr//bin/medusa-idled
	;;
esac
