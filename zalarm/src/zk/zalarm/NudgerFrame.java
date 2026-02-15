package zk.zalarm;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.Timer;

class NudgerFrame extends JFrame {
  private final JTextArea content;
  private final JLabel countDown;
  private final CountDownTimerHandler countDownTimerHandler;
  private final Timer countDownTimer;
  private final ZAlarm zalarm;
  private static final int SNOOZE_TIME_MINUTES = 5;

  NudgerFrame(ZAlarm zalarm, String title, String message, int timeoutSeconds) {
    this.zalarm = zalarm;
    setTitle(title);
    Image icon = Toolkit.getDefaultToolkit().createImage(getClass().getResource("icon.png"));
    setType(Window.Type.NORMAL);
    setResizable(false);
    setIconImage(icon);
    setLayout(new BorderLayout(5, 5));
    getContentPane().setBackground(Color.YELLOW);

    content = new JTextArea(message, 3, 50);
    content.setFont(new Font(ZAlarm.MONO_FONT_FAMILY, Font.PLAIN, 16));
    content.setEditable(false);
    content.setLineWrap(true);
    content.setWrapStyleWord(true);
    content.setMargin(new Insets(5, 5, 5, 5));
    JScrollPane jsp = new JScrollPane(content);
    jsp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    add(jsp, BorderLayout.CENTER);

    countDown = new JLabel(" ", JLabel.CENTER);
    countDown.setFont(new Font(ZAlarm.MONO_FONT_FAMILY, Font.PLAIN, 13));
    add(countDown, BorderLayout.PAGE_END);

    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    setAlwaysOnTop(true);
    pack();
    setLocationRelativeTo(null);
    setVisible(true);

    addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          zalarm.snoozeNudger(SNOOZE_TIME_MINUTES * 60);
          dispose();
        }
      });

    countDownTimerHandler = new CountDownTimerHandler(timeoutSeconds);
    countDownTimerHandler.update();
    countDownTimer = new Timer(1000, countDownTimerHandler);
    countDownTimer.start();
  }

  private class CountDownTimerHandler implements ActionListener {
    int secondsLeft;
    final LocalDateTime time = LocalDateTime.now();

    CountDownTimerHandler(int timeoutSeconds) {
      secondsLeft = timeoutSeconds;
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
      secondsLeft --;
      update();
    }

    void update() {
      if (secondsLeft > 0) {
        countDown.setText(
            "(" + secondsLeft + ") Close me to snooze for " + SNOOZE_TIME_MINUTES + "m.");
      } else {
        countDownTimer.stop();
        dispose();
      }
    }
  }
}
