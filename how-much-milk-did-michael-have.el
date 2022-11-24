(defun how-much-milk-did-michael-have()
  "Sum up all the numbers in the format like \"2oz\" or \"3.2oz\"
  within the region"
  (interactive)
  (save-excursion
    (let ((end (region-end))
          (sum 0))
      (goto-char (region-beginning))
      (while (re-search-forward "\\([0-9.]*\\)oz" end t)
        (setq sum (+ sum (string-to-number (match-string 1)))))
      (message "Total milk: %goz" sum))))
  
