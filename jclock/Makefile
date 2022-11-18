compile: src/zk/jclock/JClock.java
	javac -d compile $^
run: compile
	java -cp compile -Xmx60M -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true zk.jclock.JClock
