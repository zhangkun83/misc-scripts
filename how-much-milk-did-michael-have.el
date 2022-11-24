(defun how-much-milk-did-michael-have()
  "Sum up all the numbers in the format like \"2 oz\" or
  \"3.2oz\" within the active region or the whole file"
  (interactive)
  (save-excursion
    (let* ((use-region-p (use-region-p))
           (end (if use-region-p (region-end) nil))
           (sum 0))
      (goto-char (if use-region-p (region-beginning) 0))
      (while (re-search-forward "\\([0-9.]*\\) *oz" end t)
        (setq sum (+ sum (string-to-number (match-string 1)))))
      (message "Total of %s: %g oz"
               (if use-region-p "region" "whole file")
               sum))))
