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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

class NudgerFrame extends JFrame {
  private final JLabel content;
  private final JLabel countDown;
  private final CountDownTimerHandler countDownTimerHandler;
  private final Timer countDownTimer;
  private final ZAlarm zalarm;
  private static final int SNOOZE_TIME_MINUTES = 5;
  private final AtomicBoolean snoozeRequested = new AtomicBoolean();
  private final AtomicInteger secondsLeft = new AtomicInteger();

  NudgerFrame(ZAlarm zalarm, String title, String message, int timeoutSeconds) {
    this.zalarm = zalarm;
    setTitle(title);
    Image icon = Toolkit.getDefaultToolkit().createImage(getClass().getResource("icon.png"));
    setType(Window.Type.NORMAL);
    setResizable(false);
    setIconImage(icon);
    setLayout(new BorderLayout(5, 5));

    content =
        new JLabel("<html><div style='width: 250px;'>"
            + HtmlEscape.escapeHtml(message) + "</div></html>");
    content.setFont(new Font(ZAlarm.MONO_FONT_FAMILY, Font.PLAIN, 16));
    content.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
    content.setBackground(Color.YELLOW);
    content.setOpaque(true);
    add(content, BorderLayout.CENTER);

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
          if (snoozeRequested.compareAndSet(false, true)) {
            zalarm.snoozeNudger(SNOOZE_TIME_MINUTES * 60);
            secondsLeft.set(2);
            SwingUtilities.invokeLater(countDownTimerHandler::update);
          }
        }
      });

    countDownTimerHandler = new CountDownTimerHandler(timeoutSeconds);
    countDownTimerHandler.update();
    countDownTimer = new Timer(1000, countDownTimerHandler);
    countDownTimer.start();
  }

  private class CountDownTimerHandler implements ActionListener {
    CountDownTimerHandler(int timeoutSeconds) {
      secondsLeft.set(timeoutSeconds);
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
      secondsLeft.decrementAndGet();
      update();
    }

    void update() {
      if (secondsLeft.get() > 0) {
        if (snoozeRequested.get()) {
          countDown.setText("Snoozing for " + SNOOZE_TIME_MINUTES + " minutes ...");
        } else {
          countDown.setText(secondsLeft.toString());
        }
      } else {
        countDownTimer.stop();
        dispose();
      }
    }
  }
}
