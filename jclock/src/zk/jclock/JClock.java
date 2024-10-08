package zk.jclock;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.Timer;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;

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
    setVisible(true);
    ContentUpdater updater = new ContentUpdater();
    updater.update();
    try {
      alarmNotifier.alarm = readTimeFromFile();
    } catch (IOException e) {
      e.printStackTrace();
      setAlarm();
    }
    new Timer(1000, updater).start();
    new Timer(750, alarmNotifier).start();
  }

  private void setAlarm() {
    String input = alarmNotifier.alarm.format(dateTimeFormatterShort);
    if (alarmNotifier.alarmExpired()) {
      input = "+30";
    }
    while (true) {
      input = JOptionPane.showInputDialog(
          null, "Set alarm in the format of \"HH:MM\", \"+MM\", or \":MM\"", input);
      if (input == null) {
        return;
      }
      try {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime newAlarm;
        if (input.startsWith("+")) {
          // Relative minutes
          int deltaMinutes = Integer.parseInt(input.substring(1));
          newAlarm = now.plusMinutes(deltaMinutes);
        } else if (input.startsWith(":")) {
          // Minute part only
          int minutePart = Integer.parseInt(input.substring(1));
          newAlarm = LocalDateTime.of(
              now.getYear(), now.getMonth(), now.getDayOfMonth(),
              now.getHour(), minutePart);
          if (minutePart <= now.getMinute()) {
            // If the minute is in the past, set into the next hour
            newAlarm = newAlarm.plusHours(1);
          }
        } else {
          // Hour and minute
          String[] split = input.split(":");
          int hourPart = Integer.parseInt(split[0]);
          int minutePart = Integer.parseInt(split[1]);
          newAlarm = LocalDateTime.of(
              now.getYear(), now.getMonth(), now.getDayOfMonth(),
              hourPart, minutePart);
          // If the time of day is in the past, set into the next day
          if (newAlarm.compareTo(now) < 0) {
            newAlarm = newAlarm.plusDays(1);
          }
        }
        alarmNotifier.alarm = newAlarm;
        writeTimeToFile(newAlarm);
        JOptionPane.showMessageDialog(
            null, "Alarm set to " + formatTimeForDisplay(newAlarm, now));
        return;
      } catch (Exception e) {
        JOptionPane.showMessageDialog(
            null, "Malformed input", "Set Alarm", JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  private static String formatTimeForDisplay(LocalDateTime time, LocalDateTime now) {
    String dateString;
    String formattedDate = dateTimeFormatterDate.format(time);
    if (formattedDate.equals(dateTimeFormatterDate.format(now))) {
      dateString = "";
    } else if (formattedDate.equals(dateTimeFormatterDate.format(now.plusDays(1)))) {
      dateString = "tomorrow ";
    } else {
      dateString = formattedDate + " ";
    }
    Duration delta = Duration.between(now, time);
    long deltaMinutes = delta.toMinutes();
    long deltaAbsMinutes = Math.abs(deltaMinutes);
    long deltaMinPart = deltaAbsMinutes % 60;
    long deltaHourPart = deltaAbsMinutes / 60;
    String deltaString = "";
    if (deltaMinPart > 0) {
      deltaString = Long.toString(deltaMinPart) + "m";
    }
    if (deltaHourPart > 0) {
      deltaString = Long.toString(deltaHourPart) + "h" + deltaString;
    }
    if (delta.isNegative()) {
      deltaString = "-" + deltaString;
    } else {
      deltaString = "+" + deltaString;
    }
    return String.format(
        "%s%s (%s)",
        dateString, dateTimeFormatterShort.format(time), deltaString);
  }

  private void addMouseEvents() {
    final JPopupMenu popupMenu;
    JMenuItem miSetAlarm = new JMenuItem("Set Alarm ...");
    miSetAlarm.addActionListener((e) -> setAlarm());

    if (getOsType() == OsType.MAC) {
      popupMenu = null;
      System.setProperty("apple.laf.useScreenMenuBar", "true");
      JMenuBar menuBar = new JMenuBar();
      JMenu alarmMenu = new JMenu("Alarm");
      alarmMenu.add(miSetAlarm);
      alarmMenu.add(alarmDisplay);
      menuBar.add(alarmMenu);
      setJMenuBar(menuBar);
    } else {
      popupMenu = new JPopupMenu();
      JMenuItem miClose = new JMenuItem("Quit");
      miClose.addActionListener((e) -> System.exit(0));
      popupMenu.add(miSetAlarm);
      popupMenu.add(alarmDisplay);
      popupMenu.addSeparator();
      popupMenu.add(miClose);
    }
    MouseAdapter mouseListener = new MouseAdapter() {
        boolean isDragging = false;
        Point mouseClickPoint; // Will reference to the last pressing (not clicking) position

        @Override
        public void mousePressed(MouseEvent e) {
          if (e.getButton() == MouseEvent.BUTTON1) {
            isDragging = true;
            setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
          }
          maybeShowMenu(e);
          mouseClickPoint = e.getPoint(); // update the position
        }

        @Override
        public void mouseReleased(MouseEvent e) {
          isDragging = false;
          setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
          maybeShowMenu(e);
        }

        private void maybeShowMenu(MouseEvent e) {
          if (popupMenu != null && e.isPopupTrigger()) {
            popupMenu.show(e.getComponent(), e.getX(), e.getY());
          }
        }

        @Override
        public void mouseDragged(MouseEvent e) {
          if (!isDragging) {
            return;
          }
          Point point = e.getLocationOnScreen();
          // Keep the mouse pointer at the position relative to the window
          point.translate(-mouseClickPoint.x, -mouseClickPoint.y);
          // Make sure the whole frame is visible
          point = adjustWindowLocationForVisibility(point);
          setLocation(point); // set the new location
        }
      };
    addMouseListener(mouseListener);
    addMouseMotionListener(mouseListener);
    content.addMouseListener(mouseListener);
    content.addMouseMotionListener(mouseListener);
  }

  /**
   * Adjust the given point, representing the location of the window, if necessary to keep the
   * entirety of the window on the screen.
   */
  private Point adjustWindowLocationForVisibility(Point p) {
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension size = getBounds().getSize();
    return new Point(
        (int) (cap(p.getX(), 0, screenSize.getWidth() - size.getWidth())),
        (int) (cap(p.getY(), 0, screenSize.getHeight() - size.getHeight())));
  }

  private enum OsType {
    WINDOWS, LINUX, MAC, OTHER
  }

  private static OsType getOsType() {
    String os = System.getProperty("os.name").toLowerCase();
    if (os.indexOf("win") >= 0) {
      return OsType.WINDOWS;
    } else if (os.indexOf("mac") >= 0) {
      return OsType.MAC;
    } else if (os.indexOf("linux") >= 0) {
      return OsType.LINUX;
    } else {
      return OsType.OTHER;
    }
  }    

  private static String getFontForSystem() {
    switch (getOsType()) {
      case WINDOWS:
        return "Courier New";
      case MAC:
        return "Courier New";
      case LINUX:
        return "Noto Mono";
      default:
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

  private static Path getAlarmDataFilePath() {
    return Paths.get(System.getProperty("user.home"), ".jclock-alarm");
  }

  private static LocalDateTime readTimeFromFile() throws IOException {
    String content = new String(Files.readAllBytes(getAlarmDataFilePath()));
    return LocalDateTime.parse(content, dateTimeFormatterFull);
  }

  private static void writeTimeToFile(LocalDateTime time) throws IOException {
    Files.write(getAlarmDataFilePath(), time.format(dateTimeFormatterFull).getBytes());
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
      String alarmInfo = "Alarm: " + formatTimeForDisplay(alarmNotifier.alarm, LocalDateTime.now());
      alarmDisplay.setText(alarmInfo);
      content.setToolTipText(alarmInfo);

      // Make sure the window is still visible (in case screen resolution has changed etc).
      Point location = getLocationOnScreen();
      Point adjustedLocation = adjustWindowLocationForVisibility(location);
      if (!location.equals(adjustedLocation)) {
        setLocation(adjustedLocation);
      }
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
    ToolTipManager toolTipManager = ToolTipManager.sharedInstance();
    toolTipManager.setEnabled(true);
    // By default tooltips are displayed only for active (focused) windows.  This option will make
    // tooltips displayed for all windows regardless of focus.
    UIManager.put("ToolTipManager.enableToolTipMode", "allWindows");
    toolTipManager.setInitialDelay(0);
    toolTipManager.setDismissDelay(Integer.MAX_VALUE);
    JClock instance = new JClock();
    instance.start();
  }
}
