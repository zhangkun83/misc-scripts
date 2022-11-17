package zk.jclock;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.Timer;

/**
 * A minimalist digital clock that is always visible
 */
public class JClock extends JFrame {
  private final JLabel content;
  private Point mouseClickPoint; // Will reference to the last pressing (not clicking) position

  private JClock() {
    // This will make the window skip the window switcher and on all virtual desktops
    setType(Type.UTILITY);
    setLayout(new FlowLayout());
    getContentPane().setBackground(Color.YELLOW);
    content = new JLabel("");
    content.setFont(new Font(Font.MONOSPACED, Font.BOLD, 12));
    add(content);

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setAlwaysOnTop(true);
    setUndecorated(true);
    setResizable(false);
    addEventsForDragging();
    setVisible(true);
    ContentUpdater updater = new ContentUpdater();
    updater.update();
    new Timer(1000, updater).start();
  }

  private void addEventsForDragging() {
    // Here is the code does moving
    addMouseListener(new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          mouseClickPoint = e.getPoint(); // update the position
        }

      });
    addMouseMotionListener(new MouseAdapter() {
        @Override
        public void mouseDragged(MouseEvent e) {
          Point point = e.getLocationOnScreen();
          point.translate(-mouseClickPoint.x, -mouseClickPoint.y); // Moves the point by given values from its location
          // Make sure the whole frame is visible
          Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
          Dimension size = getBounds().getSize();
          point.setLocation(
              cap(point.getX(), 0, screenSize.getWidth() - size.getWidth()),
              cap(point.getY(), 0, screenSize.getHeight() - size.getHeight()));
          setLocation(point); // set the new location
        }
      });
  }

  private static double cap(double value, double min, double max) {
    if (value < min) {
      value = min;
    }
    if (value > max) {
      value = max;
    }
    return value;
  }

  private class ContentUpdater implements ActionListener {
    boolean showColon;

    @Override
    public void actionPerformed(ActionEvent evt) {
      update();
    }

    void update() {
      String formatString = showColon ? "HH:mm" : "HH mm";
      content.setText(
          new SimpleDateFormat(formatString).format(new Date()));
      pack();
      showColon = !showColon;
    }
  }

  public static void main(String[] args) {
    JClock instance = new JClock();
  }
}
