package zk.jcurtain;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 * A Curtain to block off rectangular areas on the screen.
 */
public class JCurtain extends JFrame {
  private JCurtain(int x, int y, int width, int height, Color color) {
    // This will make the window skip the window switcher and on all virtual desktops
    setType(Type.UTILITY);
    getContentPane().setBackground(color);
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setAlwaysOnTop(true);
    setUndecorated(true);
    setLocation(new Point(x, y));
    setSize(width, height);
    setResizable(false);
    addMouseEvents();
    setVisible(true);
  }

  private void addMouseEvents() {
    final JPopupMenu menu = new JPopupMenu();
    JMenuItem miClose = new JMenuItem("Close curtains");
    miClose.addActionListener((e) -> System.exit(0));
    menu.add(miClose);
    addMouseListener(new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          maybeShowMenu(e);
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
      });
  }


  public static void main(String[] args) {
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    System.err.println("Screen size: " + screenSize);
    ArrayList<String> posArgs = new ArrayList<String>();
    ArrayList<String> flags = new ArrayList<String>();
    for (String arg : args) {
      if (arg.startsWith("--")) {
        flags.add(arg);
      } else {
        posArgs.add(arg);
      }
    }
    Color color = Color.BLACK;
    for (String flag : flags) {
      if (flag.startsWith("--color=")) {
        String colorString = flag.substring("--color=".length());
        color = Color.decode(colorString);
      }
    }
    if (posArgs.isEmpty()) {
      int screenWidth = (int) screenSize.getWidth();
      int screenHeight = (int) screenSize.getHeight();
      int jdosboxWidth = screenHeight / 3 * 4;
      int curtainWidth = (screenWidth - jdosboxWidth) / 2;
      JCurtain left = new JCurtain(0, 0, curtainWidth, screenHeight, color);
      JCurtain right =
          new JCurtain(curtainWidth + jdosboxWidth, 0, curtainWidth, screenHeight, color);
    } else {
      for (String arg : posArgs) {
        String[] split = arg.split(":");
        JCurtain curtain =
            new JCurtain(
                Integer.parseInt(split[0]),
                Integer.parseInt(split[1]),
                Integer.parseInt(split[2]),
                Integer.parseInt(split[3]),
                color);
      }
    }
  }
}
