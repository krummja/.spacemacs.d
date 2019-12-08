;;; s-utils --- utility functions

(require 'dash-functional)

;;;###autoload
(defun s-utils/add-hooks (hooks fns)
  (dolist (hook hooks)
    (dolist (fn fns)
      (add-hook hook fn t))))

(provide 's-utils)
