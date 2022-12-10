jdosbox-curtain.jar: src/zk/jdosboxcurtain/JdosboxCurtain.java
	javac -d compile $^ && jar cfe $@ zk.jdosboxcurtain.JdosboxCurtain -C compile .
run: jdosbox-curtain.jar
	java -Xmx60M -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -jar jdosbox-curtain.jar
