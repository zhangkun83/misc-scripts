package zk.zalarm;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

/**
 * A desktop alarm program.
 */
public class ZAlarm {
  private static final String TITLE = "Z Alarm";
  private static final String TIME_FORMAT = "HH:mm";
  private static final Color BG_LIGHTED = Color.YELLOW;
  private static final DateTimeFormatter dateTimeFormatterShort =
      DateTimeFormatter.ofPattern(TIME_FORMAT);
  private static final DateTimeFormatter dateTimeFormatterFull =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
  static final String MONO_FONT_FAMILY = "Aporetic Sans Mono";
  private static final Font LARGE_FONT_BOLD = new Font(MONO_FONT_FAMILY, Font.BOLD, 20);
  private static final Font LARGE_FONT = new Font(MONO_FONT_FAMILY, Font.PLAIN, 20);
  private static final Font FONT_BOLD = new Font(MONO_FONT_FAMILY, Font.BOLD, 13);
  private static final Font FONT = new Font(MONO_FONT_FAMILY, Font.PLAIN, 13);
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
    long dayDifference =
        ChronoUnit.DAYS.between(now.with(LocalTime.MIN), time.with(LocalTime.MIN));
    String dayString = "";
    if (dayDifference > 0) {
      dayString = "(+" + dayDifference + "d)";
    } else if (dayDifference < 0) {
      dayString = "(" + dayDifference + "d)";
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
      deltaString = deltaString.length() > 0 ? deltaString + " ago" : "just now";
    } else {
      deltaString = deltaString.length() > 0 ? "in " + deltaString : "imminent";
    }
    return String.format(
        "%s%s (%s)",
        dateTimeFormatterShort.format(time), dayString, deltaString);
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
    final JButton setAlarmCancelButton;
    final JPanel setAlarmPanel;
    final JTextField setAlarmInput;
    final JTextField setAlarmMessageInput;

    MainFrame() {
      Container contentPane = getContentPane();
      setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
      setIconImage(icon);
      setResizable(false);
      setTitle(TITLE);

      setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
          @Override
          public void windowClosing(WindowEvent e) {
            int confirmed = JOptionPane.showConfirmDialog(
                MainFrame.this,
                "Terminate Z Alarm?",
                "Confirmation",
                JOptionPane.YES_NO_OPTION);

            if (confirmed == JOptionPane.YES_OPTION) {
              System.exit(0);
            }
          }
        });

      addWindowFocusListener(new WindowFocusListener() {
          @Override
          public void windowGainedFocus(WindowEvent e) {
            pack();
          }

          @Override
          public void windowLostFocus(WindowEvent e) {}
        });

      JPanel clockPanel = new FixedWidthPanel();
      clockPanel.setBorder(createEmptyPanelBorder());
      clockPanel.setLayout(new BoxLayout(clockPanel, BoxLayout.Y_AXIS));
      JLabel nowLabel = new JLabel("Now");
      nowLabel.setFont(FONT);
      clockPanel.add(nowLabel);
      clockPanel.add(timeLabel = new JLabel());
      clockPanel.add(dateLabel = new JLabel());
      dateLabel.setFont(FONT);
      timeLabel.setFont(LARGE_FONT);
      add(clockPanel);
 
      JPanel alarmPanel = new FixedWidthPanel();
      alarmPanel.setBorder(createEmptyPanelBorder());
      alarmPanel.setLayout(new BoxLayout(alarmPanel, BoxLayout.Y_AXIS));
      alarmPanel.add(alarmMessageLabel = new JLabel());
      alarmPanel.add(alarmLabel = new JLabel());
      alarmMessageLabel.setFont(FONT_BOLD);
      alarmLabel.setFont(LARGE_FONT_BOLD);
      alarmPanel.add(setAlarmButton = new JButton("Set"));
      setAlarmButton.setFont(FONT);
      add(alarmPanel);

      setAlarmPanel = new FixedWidthPanel();
      setAlarmPanel.setBorder(createEmptyPanelBorder());
      setAlarmPanel.setLayout(new BoxLayout(setAlarmPanel, BoxLayout.Y_AXIS));
      JLabel timeDescriptionLabel = new JLabel("Time (\"HH:MM\", \"+MM\", or \":MM\")");
      timeDescriptionLabel.setFont(FONT);
      setAlarmPanel.add(timeDescriptionLabel);
      setAlarmPanel.add(setAlarmInput = new JTextField());
      setAlarmInput.addFocusListener(textFocusSelectAllListener);
      setAlarmInput.setFont(FONT_BOLD);
      setAlarmPanel.add(Box.createVerticalStrut(5));
      JLabel messageDecriptionLabel = new JLabel("Message (optional)");
      messageDecriptionLabel.setFont(FONT);
      setAlarmPanel.add(messageDecriptionLabel);
      setAlarmPanel.add(setAlarmMessageInput = new JTextField());
      setAlarmMessageInput.addFocusListener(textFocusSelectAllListener);
      setAlarmMessageInput.setFont(FONT_BOLD);
      setAlarmPanel.add(Box.createVerticalStrut(5));
      setAlarmPanel.add(setAlarmSubmitButton = new JButton("OK"));
      setAlarmSubmitButton.setFont(FONT);
      setAlarmPanel.add(Box.createVerticalStrut(5));
      setAlarmPanel.add(setAlarmCancelButton = new JButton("Cancel"));
      setAlarmCancelButton.setFont(FONT);
      add(setAlarmPanel);

      hideSetAlarmPanel();

      setAlarmButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            if (alarm.isExpired()) {
              setAlarmInput.setText("+30");
              setAlarmMessageInput.setText("");
            } else {
              setAlarmInput.setText(dateTimeFormatterShort.format(alarm.time));
              setAlarmMessageInput.setText(alarm.message);
            }
            setAlarmButton.setEnabled(false);
            setAlarmPanel.setVisible(true);
            setAlarmInput.requestFocusInWindow();
            getRootPane().setDefaultButton(setAlarmSubmitButton);
            pack();
          }
        });

      setAlarmSubmitButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent ae) {
            try {
              applyNewAlarm(setAlarmInput.getText(), setAlarmMessageInput.getText());
              hideSetAlarmPanel();
              contentUpdater.update();
              pack();
            } catch (Exception e) {
              JOptionPane.showMessageDialog(
                  MainFrame.this, "Malformed input", "Set Alarm", JOptionPane.ERROR_MESSAGE);
            }
          }
        });

      setAlarmCancelButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent ae) {
            hideSetAlarmPanel();
            pack();
          }
        });

      // Bind Escape key to the "Cancel" button
      Action cancelAction = new AbstractAction() {
          @Override
          public void actionPerformed(ActionEvent e) {
            setAlarmCancelButton.doClick(); // Trigger the button's action listeners
          }
        };
      InputMap inputMap = rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
      ActionMap actionMap = rootPane.getActionMap();
      inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "cancel");
      actionMap.put("cancel", cancelAction);      
    }

    private void hideSetAlarmPanel() {
      setAlarmPanel.setVisible(false);
      setAlarmButton.setEnabled(true);
      getRootPane().setDefaultButton(setAlarmButton);
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
      mainFrame.setTitle(TITLE + " *" + alarmInfo);
      alarmInfo =
          "<html><nobr>"
          + alarmInfo
              .replaceAll("\\(", "<span style='font-size: 10px;'>(")
              .replaceAll("\\)", ")</span>")
          + "</nobr></html>";
      mainFrame.alarmLabel.setText(alarmInfo);
      // <html> to allow wrapping text in JLabel
      mainFrame.alarmMessageLabel.setText(
          "<html>" + HtmlEscape.escapeHtml(alarm.getDisplayedMessage()) +"</html>");
    }
  }

  private class AlarmNotifier implements ActionListener {
    private static final int NUDGER_TIMEOUT_SECONDS = 50;
    private static final int NUDGE_INTERVAL_SECONDS = NUDGER_TIMEOUT_SECONDS + 10;
    private static final Duration NUDGE_INTERVAL = Duration.ofSeconds(NUDGE_INTERVAL_SECONDS);
    boolean lighted;

    @Override
    public void actionPerformed(ActionEvent evt) {
      AlarmInfo alarm = ZAlarm.this.alarm;
      LocalDateTime now = LocalDateTime.now();
      if (alarm.isExpired() && now.compareTo(nextNudgeTime) > 0) {
        StringBuilder message = new StringBuilder();
        if (alarm.message.length() > 0) {
          message.append(alarm.message);
        } else {
          message.append("Alarm expired");
        }
        String alarmString = formatTimeForDisplay(alarm.time, now);
        message.append(" at ").append(alarmString);
        disposeNudger();
        currentNudger =
            new NudgerFrame(
                ZAlarm.this, "Alarm Expired", message.toString(), NUDGER_TIMEOUT_SECONDS);
        nextNudgeTime = now.plus(NUDGE_INTERVAL);
      }

      if (alarm.isExpired() || lighted) {
        lighted = !lighted;
        JLabel alarmLabel = mainFrame.alarmLabel;
        if (lighted) {
          alarmLabel.setBackground(BG_LIGHTED);
          alarmLabel.setOpaque(true);
        } else {
          alarmLabel.setOpaque(false);
          alarmLabel.setBackground(null);
        }
      }
    }
  }

  private final ContentUpdater contentUpdater = new ContentUpdater();
  private final AlarmNotifier alarmNotifier = new AlarmNotifier();
  private final FocusAdapter textFocusSelectAllListener =
      new FocusAdapter() {
        @Override
        public void focusGained(FocusEvent e) {
          if (e.getComponent() instanceof JTextField) {
            JTextField textField = (JTextField) e.getComponent();
            SwingUtilities.invokeLater(() -> textField.selectAll());
          }
        }
      };
  private MainFrame mainFrame;
  private AlarmInfo alarm = new AlarmInfo();
  private LocalDateTime nextNudgeTime = LocalDateTime.MAX;
  private NudgerFrame currentNudger;

  private static class AlarmInfo {
    final LocalDateTime time;
    final String message;

    AlarmInfo(LocalDateTime time, String message) {
      this.time = time;
      this.message = message;
    }

    AlarmInfo() {
      this.time = LocalDateTime.now();
      this.message = "";
    }

    String getDisplayedMessage() {
      if (message.trim().length() == 0) {
        return "Alarm";
      } else {
        return message.trim();
      }
    }

    boolean isExpired() {
      return LocalDateTime.now().compareTo(time) > 0;
    }
  }

  private void applyNewAlarm(String input, String messageInput) throws Exception {
    LocalDateTime now = LocalDateTime.now();
    LocalDateTime newAlarm;
    input = input.trim();
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
    setAlarm(new AlarmInfo(newAlarm, messageInput));
  }

  void snoozeNudger(int seconds) {
    SwingUtilities.invokeLater(() -> {
          if (alarm.isExpired()) {
            nextNudgeTime = LocalDateTime.now().plusSeconds(seconds);
          }
        });
  }

  void disposeNudger() {
    NudgerFrame nudger = currentNudger;
    if (nudger != null) {
      nudger.dispose();
    }
  }

  private void setAlarm(AlarmInfo newAlarm) {
    alarm = newAlarm;
    nextNudgeTime = LocalDateTime.MIN;
    disposeNudger();
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
    Timer updateTimer = new Timer(1000, contentUpdater);
    updateTimer.start();
    Timer alarmNotifierTimer = new Timer(750, alarmNotifier);
    alarmNotifierTimer.start();
    mainFrame.setVisible(true);
  }

  public static void main(String[] args) {
    ZAlarm instance = new ZAlarm();
    instance.start();
  }
}
