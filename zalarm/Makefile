zalarm.jar: src/zk/zalarm/ZAlarm.java
	javac -d compile $^ && cp icon.png compile/zk/zalarm/ && jar cfe $@ zk.zalarm.ZAlarm -C compile .
clean:
	rm -r zalarm.jar compile
run: zalarm.jar
	java -Xmx60M -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -jar zalarm.jar
