compile: src/zk/jclock/JClock.java
	javac -d compile $^
run: compile
	java -cp compile -Xmx60M zk.jclock.JClock
