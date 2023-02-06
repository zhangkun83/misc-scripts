jcurtain.jar: src/zk/jcurtain/JCurtain.java
	mkdir -p compile && javac -d compile $^ && jar cfe $@ zk.jcurtain.JCurtain -C compile .
run: jcurtain.jar
	java -Xmx20M -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -jar jcurtain.jar
