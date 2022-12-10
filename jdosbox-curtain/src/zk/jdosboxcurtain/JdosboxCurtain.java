package zk.jdosboxcurtain;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 * A Curtain to cover up the white areas on the left and right of jdosbox.
 */
public class JdosboxCurtain extends JFrame {
  private enum Side {
    LEFT, RIGHT
  }

  private JdosboxCurtain(Side side) {
    // This will make the window skip the window switcher and on all virtual desktops
    setType(Type.UTILITY);
    getContentPane().setBackground(Color.BLACK);

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setAlwaysOnTop(true);
    setUndecorated(true);
    setResizable(false);
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    int screenWidth = (int) screenSize.getWidth();
    int screenHeight = (int) screenSize.getHeight();
    int jdosboxWidth = screenHeight / 3 * 4;
    int curtainWidth = (screenWidth - jdosboxWidth) / 2;
    switch (side) {
      case LEFT:
        setLocation(new Point(0, 0));
        break;
      case RIGHT:
        setLocation(new Point(curtainWidth + jdosboxWidth, 0));
        break;
    }
    setSize(curtainWidth, screenHeight);
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
    JdosboxCurtain left = new JdosboxCurtain(Side.LEFT);
    JdosboxCurtain right = new JdosboxCurtain(Side.RIGHT);
  }
}
