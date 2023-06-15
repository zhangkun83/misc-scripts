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
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.Timer;

/**
 * A minimalist digital clock that is always visible
 */
public class JClock extends JFrame {
  private static final Color BG_NORMAL = Color.BLACK;
  private static final Color BG_LIGHTED = Color.WHITE;
  private static final String DATE_FORMAT = "yyyy-MM-dd";
  private static final String TIME_FORMAT = "HH:mm";
  private static final DateTimeFormatter dateTimeFormatterDate =
      DateTimeFormatter.ofPattern(DATE_FORMAT);
  private static final DateTimeFormatter dateTimeFormatterShort =
      DateTimeFormatter.ofPattern(TIME_FORMAT);
  private static final DateTimeFormatter dateTimeFormatterFull =
      DateTimeFormatter.ofPattern(DATE_FORMAT + " " + TIME_FORMAT);
  private final JLabel content;
  private final JMenuItem alarmDisplay;
  private final AlarmNotifier alarmNotifier = new AlarmNotifier();
  private Point mouseClickPoint; // Will reference to the last pressing (not clicking) position

  private JClock() {
    // This will make the window skip the window switcher and on all virtual desktops
    setType(Type.UTILITY);
    setLayout(new FlowLayout(FlowLayout.CENTER, 2, 2));
    getContentPane().setBackground(BG_NORMAL);
    content = new JLabel("");
    content.setFont(new Font(getFontForSystem(), Font.BOLD, 12));
    content.setForeground(Color.GREEN);
    add(content);

    alarmDisplay = new JMenuItem();
    alarmDisplay.setEnabled(false);

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
    new Timer(750, alarmNotifier).start();
    setVisible(true);
    setAlarm();
  }

  private void setAlarm() {
    String input = alarmNotifier.alarm.format(dateTimeFormatterShort);
    if (alarmNotifier.alarmExpired()) {
      input = LocalDateTime.now().plusMinutes(30).format(dateTimeFormatterShort);
    }
    while (true) {
      input = JOptionPane.showInputDialog(
          this, "Set alarm in the format of HH:MM or +MM", input);
      if (input == null) {
        return;
      }
      try {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime newAlarm;
        if (input.startsWith("+")) {
          int deltaMinutes = Integer.parseInt(input.substring(1));
          newAlarm = now.plusMinutes(deltaMinutes);
        } else {
          newAlarm = LocalDateTime.parse(
              now.format(dateTimeFormatterDate) + " " + input, dateTimeFormatterFull);
          if (newAlarm.compareTo(now.minusHours(1)) < 0) {
            if (JOptionPane.showConfirmDialog(
                    this,
                    "The alarm will be set to tomorrow at " + input + ". Are you sure?",
                    "Set Alarm",
                    JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
              newAlarm = newAlarm.plusDays(1);
            } else {
              return;
            }
          }
        }
        alarmNotifier.alarm = newAlarm;
        return;
      } catch (Exception e) {
        JOptionPane.showMessageDialog(
            this, "Invalid time format: " + e.getMessage(), "Set Alarm", JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  private static String formatTimeForDisplay(LocalDateTime time, LocalDateTime now) {
    String dateString;
    String formattedDate = dateTimeFormatterDate.format(time);
    if (formattedDate.equals(dateTimeFormatterDate.format(now))) {
      dateString = "";
    } else if (formattedDate.equals(dateTimeFormatterDate.format(now.plusDays(1)))) {
      dateString = "Tomorrow";
    } else {
      dateString = formattedDate;
    }
    long deltaMinutes = Duration.between(now, time).toMinutes();
    return String.format(
        "%s %s (%+dm)",
        dateString, dateTimeFormatterShort.format(time), deltaMinutes);
  }

  private void addMouseEvents() {
    final JPopupMenu menu = new JPopupMenu();
    JMenuItem miSetAlarm = new JMenuItem("Set Alarm");
    miSetAlarm.addActionListener((e) -> setAlarm());
    JMenuItem miClose = new JMenuItem("Close");
    miClose.addActionListener((e) -> System.exit(0));
    menu.add(miSetAlarm);
    menu.add(alarmDisplay);
    menu.addSeparator();
    menu.add(miClose);
    MouseAdapter mouseListener = new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          maybeShowMenu(e);
          mouseClickPoint = e.getPoint(); // update the position
        }

        @Override
        public void mouseReleased(MouseEvent e) {
          maybeShowMenu(e);
        }

        private void maybeShowMenu(MouseEvent e) {
          if (e.isPopupTrigger()) {
            menu.show(e.getComponent(), e.getX(), e.getY());
          }
        }
      };
    MouseAdapter mouseMotionListener = new MouseAdapter() {
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
      };
    addMouseListener(mouseListener);
    addMouseMotionListener(mouseMotionListener);
    content.addMouseListener(mouseListener);
    content.addMouseMotionListener(mouseMotionListener);
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
      String formatString = showColon ? "HH:mm" : "HH mm";
      content.setText(
          new SimpleDateFormat(formatString).format(new Date()));
      pack();
      showColon = !showColon;
      alarmDisplay.setText(
          "Alarm: " + formatTimeForDisplay(alarmNotifier.alarm, LocalDateTime.now()));
    }
  }

  private class AlarmNotifier implements ActionListener {
    boolean lighted;
    LocalDateTime alarm = LocalDateTime.now();

    @Override
    public void actionPerformed(ActionEvent evt) {
      if (alarmExpired() || lighted) {
        lighted = !lighted;
        getContentPane().setBackground(lighted ? BG_LIGHTED : BG_NORMAL);
      }
    }

    private boolean alarmExpired() {
      return LocalDateTime.now().compareTo(alarm) > 0;
    }
  }

  public static void main(String[] args) {
    JClock instance = new JClock();
    instance.start();
  }
}
