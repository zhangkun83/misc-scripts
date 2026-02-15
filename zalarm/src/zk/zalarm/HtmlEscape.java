package zk.zalarm;

final class HtmlEscape {

  /**
   * Escapes a string for use in HTML to prevent XSS and ensure proper rendering.
   * Replaces special characters with HTML entities.
   * @param unsafeString The unescaped string.
   * @return The HTML escaped string.
   */
  public static String escapeHtml(String unsafeString) {
    if (unsafeString == null) {
      return null;
    }

    StringBuilder safeString = new StringBuilder();
    for (char c : unsafeString.toCharArray()) {
      switch (c) {
        case '<':
          safeString.append("&lt;");
          break;
        case '>':
          safeString.append("&gt;");
          break;
        case '&':
          safeString.append("&amp;");
          break;
        case '"':
          safeString.append("&quot;");
          break;
        case '\'':
          // Note: ' can be represented as &#39;
          safeString.append("&#39;");
          break;
        default:
          safeString.append(c);
          break;
      }
    }
    return safeString.toString();
  }
}
