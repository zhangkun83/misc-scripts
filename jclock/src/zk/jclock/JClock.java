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
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
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
    setLayout(new FlowLayout(FlowLayout.CENTER, 2, 2));
    getContentPane().setBackground(Color.BLACK);
    content = new JLabel("");
    content.setFont(new Font(getFontForSystem(), Font.BOLD, 12));
    content.setForeground(Color.GREEN);
    add(content);

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setAlwaysOnTop(true);
    setUndecorated(true);
    setResizable(false);
    addMouseEvents();
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    setLocation(new Point((int) (screenSize.getWidth() * 3 / 4), 0));
  }

  private void start() {
    ContentUpdater updater = new ContentUpdater();
    updater.update();
    new Timer(1000, updater).start();
    setVisible(true);
  }

  private void addMouseEvents() {
    final JPopupMenu menu = new JPopupMenu();
    JMenuItem miClose = new JMenuItem("Close");
    miClose.addActionListener((e) -> System.exit(0));
    menu.add(miClose);
    addMouseListener(new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          if (e.isPopupTrigger()) {
            menu.show(e.getComponent(), e.getX(), e.getY());
          }
          mouseClickPoint = e.getPoint(); // update the position
        }
      });
    addMouseMotionListener(new MouseAdapter() {
        @Override
        public void mouseDragged(MouseEvent e) {
          Point point = e.getLocationOnScreen();
          // Moves the point by given values from its location
          point.translate(-mouseClickPoint.x, -mouseClickPoint.y);
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

  private static String getFontForSystem() {
    String os = System.getProperty("os.name").toLowerCase();
    if (os.indexOf("win") >= 0) {
      return "Courier New";
    } else if (os.indexOf("linux") >= 0) {
      return "Noto Mono";
    } else {
      return Font.MONOSPACED;
    }
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
      String formatString = showColon ? "hh:mm" : "hh mm";
      content.setText(
          new SimpleDateFormat(formatString).format(new Date()));
      pack();
      showColon = !showColon;
    }
  }

  public static void main(String[] args) {
    JClock instance = new JClock();
    instance.start();
  }
}
