package zk.zalarm;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

/**
 * A desktop alarm program.
 */
public class ZAlarm {
  private static final String DATE_FORMAT = "E, MMM dd";
  private static final String TIME_FORMAT = "HH:mm";
  private static final DateTimeFormatter dateTimeFormatterDate =
      DateTimeFormatter.ofPattern(DATE_FORMAT);
  private static final DateTimeFormatter dateTimeFormatterShort =
      DateTimeFormatter.ofPattern(TIME_FORMAT);
  private static final DateTimeFormatter dateTimeFormatterFull =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
  private static final Font timeFont = new Font("Aporetic Sans Mono", Font.BOLD, 20);
  private final Image icon;

  ZAlarm() {
    icon = Toolkit.getDefaultToolkit().createImage(getClass().getResource("icon.png"));
  }

  private static Border createEmptyPanelBorder() {
    return BorderFactory.createEmptyBorder(10, 10, 10, 10);
  }

  private static Path getAlarmDataFilePath() {
    return Paths.get(System.getProperty("user.home"), ".zalarm-save");
  }

  private static AlarmInfo readAlarmFromFile() throws Exception {
    List<String> lines = Files.readAllLines(getAlarmDataFilePath(), StandardCharsets.UTF_8);
    return new AlarmInfo(
        LocalDateTime.parse(lines.get(0), dateTimeFormatterFull),
        lines.get(1));
  }

  private static void writeAlarmToFile(AlarmInfo alarm) throws IOException {
    Files.write(getAlarmDataFilePath(),
        Arrays.asList(
            alarm.time.format(dateTimeFormatterFull),
            alarm.message));
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

  private static final int UI_WIDTH = 250;

  private static class FixedWidthPanel extends JPanel {
    @Override
    public Dimension getPreferredSize() {
        Dimension size = super.getPreferredSize();
        size.width = UI_WIDTH;
        // size.height remains untouched
        return size;
    }    
  }

  private class MainFrame extends JFrame {
    final JLabel dateLabel;
    final JLabel timeLabel;
    final JLabel alarmLabel;
    final JLabel alarmMessageLabel;
    final JButton setAlarmButton;
    final JButton setAlarmSubmitButton;
    final JPanel setAlarmPanel;
    final JTextField setAlarmInput;
    final JTextField setAlarmMessageInput;

    MainFrame() {
      Container contentPane = getContentPane();
      setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
      setIconImage(icon);
      setResizable(true);
      setTitle("Z Alarm");

      JPanel clockPanel = new FixedWidthPanel();
      clockPanel.setBorder(createEmptyPanelBorder());
      clockPanel.setLayout(new BoxLayout(clockPanel, BoxLayout.Y_AXIS));
      clockPanel.add(dateLabel = new JLabel());
      clockPanel.add(timeLabel = new JLabel());
      timeLabel.setFont(timeFont);
      add(clockPanel);
 
      JPanel alarmPanel = new FixedWidthPanel();
      alarmPanel.setBorder(createEmptyPanelBorder());
      alarmPanel.setLayout(new BoxLayout(alarmPanel, BoxLayout.Y_AXIS));
      alarmPanel.add(alarmMessageLabel = new JLabel());
      alarmPanel.add(alarmLabel = new JLabel());
      alarmLabel.setFont(timeFont);
      alarmPanel.add(setAlarmButton = new JButton("Set"));
      add(alarmPanel);
      getRootPane().setDefaultButton(setAlarmButton);

      setAlarmPanel = new FixedWidthPanel();
      setAlarmPanel.setBorder(createEmptyPanelBorder());
      setAlarmPanel.setLayout(new BoxLayout(setAlarmPanel, BoxLayout.Y_AXIS));
      setAlarmPanel.add(new JLabel("Time (\"HH:MM\", \"+MM\", or \":MM\")"));
      setAlarmPanel.add(setAlarmInput = new JTextField());
      setAlarmPanel.add(Box.createVerticalStrut(5));
      setAlarmPanel.add(new JLabel("Message (optional)"));
      setAlarmPanel.add(setAlarmMessageInput = new JTextField());
      setAlarmPanel.add(setAlarmSubmitButton = new JButton("Submit"));
      add(setAlarmPanel);
      setAlarmPanel.setVisible(false);

      setAlarmButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            setAlarmButton.setEnabled(false);
            setAlarmPanel.setVisible(true);
            setAlarmInput.requestFocusInWindow();
            getRootPane().setDefaultButton(setAlarmSubmitButton);
            pack();
          }
        });

      setAlarmSubmitButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            setAlarmPanel.setVisible(false);
            setAlarmButton.setEnabled(true);
            getRootPane().setDefaultButton(setAlarmButton);
            pack();
          }
        });
    }
  }

  class ContentUpdater implements ActionListener {
    boolean showColon;
    final SimpleDateFormat dateFormat = new SimpleDateFormat("E, MMM dd");
    final SimpleDateFormat timeFormatWithColon = new SimpleDateFormat("HH:mm");
    final SimpleDateFormat timeFormatWithoutColon = new SimpleDateFormat("HH mm");

    @Override
    public void actionPerformed(ActionEvent evt) {
      update();
    }

    void update() {
      SimpleDateFormat clockFormat = showColon ? timeFormatWithColon : timeFormatWithoutColon;
      Date date = new Date();
      mainFrame.dateLabel.setText(dateFormat.format(date));
      mainFrame.timeLabel.setText(clockFormat.format(date));
      showColon = !showColon;

      String alarmInfo = formatTimeForDisplay(alarm.time, LocalDateTime.now());
      mainFrame.alarmLabel.setText(alarmInfo);
      mainFrame.alarmMessageLabel.setText(alarm.message);
    }
  }

  private final ContentUpdater contentUpdater = new ContentUpdater();
  private MainFrame mainFrame;
  private AlarmInfo alarm = new AlarmInfo();

  private static class AlarmInfo {
    final LocalDateTime time;
    final String message;

    AlarmInfo(LocalDateTime time, String message) {
      this.time = time;
      this.message = message;
    }

    AlarmInfo() {
      this.time = LocalDateTime.now();
      this.message = "Alarm";
    }
  }
  

  private void setAlarm(AlarmInfo newAlarm) {
    alarm = newAlarm;
    try {
      writeAlarmToFile(newAlarm);
    } catch (IOException e) {
      System.err.println("Could not save alarm: " + e);
    }
  }
  
  private void start() {
    mainFrame = new MainFrame();
    AlarmInfo savedAlarm;
    try {
      savedAlarm = readAlarmFromFile();
    } catch (Exception e) {
      System.err.println("Could not load saved alarm: " + e);
      savedAlarm = new AlarmInfo();
    }
    setAlarm(savedAlarm);
    contentUpdater.update();
    mainFrame.pack();
    mainFrame.setVisible(true);
    Timer updateTimer = new Timer(1000, contentUpdater);
    updateTimer.start();
  }

  public static void main(String[] args) {
    ZAlarm instance = new ZAlarm();
    instance.start();
  }
}
