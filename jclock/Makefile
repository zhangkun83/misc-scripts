jclock.jar: src/zk/jclock/JClock.java
	javac -d compile $^ && jar cfe $@ zk.jclock.JClock -C compile .
run: jclock.jar
	java -Xmx60M -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -jar jclock.jar
