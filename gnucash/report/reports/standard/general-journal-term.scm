;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-journal-term.scm: general journal report with period
;; 
;; By David Montenegro <sunrise2000@comcast.net> 2004.07.14
;; 
;;  * BUGS:
;;    
;;    See any "FIXME"s in the code.
;;    
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard general-journal-term))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define reportname (N_ "Periodic General Journal"))
(define regrptname (N_ "Register"))
(define regrptguid "22104e02654c4adba844ee75a3f8d173")
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

;; options generator

(define (general-journal-term-options-generator)

  (let* ((options (gnc:report-template-new-options/report-guid regrptguid regrptname)))

    (define (set-option! section name value)
      (GncOption-set-default-value
       (gnc-lookup-option (gnc:optiondb options) section name) value))

    (gnc-register-simple-boolean-option options
      gnc:pagename-display (N_ "Use Short Account Name")
      "ga" (G_ "Display the short account name?") #f)

    ;; set the "__reg" options required by the Register Report...
    (for-each
     (lambda (l)
       (set-option! "__reg" (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list "journal" #t)
      (list "double" #t)
      (list "debit-string" (G_ "Debit"))
      (list "credit-string" (G_ "Credit"))))

    ;; set options in the display tab...
    (for-each
     (lambda (l)
       (set-option! gnc:pagename-display (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list (N_ "Date") #t)
      (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
          (list (N_ "Num/Action") #f)
          (list (N_ "Num") #f))
      (list (N_ "Description") #t)
      (list (N_ "Account") #t)
      (list (N_ "Use Short Account Name") #f)
      (list (N_ "Shares") #f)
      (list (N_ "Price") #f)
      ;; note the "Amount" multichoice option here
      (list (N_ "Amount") 'double)
      (list (N_ "Running Balance") #f)
      (list (N_ "Totals") #f)))

    (set-option! gnc:pagename-general gnc:optname-reportname (G_ reportname))
    (set-option! gnc:pagename-general "Title" (G_ reportname))
    (gnc:options-add-date-interval!
      options gnc:pagename-general
      optname-from-date optname-to-date "a")
    options))

;; report renderer

(define (general-journal-term-renderer report-obj)
  (let ((renderer (gnc:report-template-renderer/report-guid regrptguid #f))
    (query (qof-query-create-for-splits)))

    (define (get-option section name)
      (gnc-optiondb-lookup-value (gnc:report-options report-obj) section name))

    ;; Match, by default, all non-void transactions ever recorded in
    ;; all accounts....  Whether or not to match void transactions,
    ;; however, may be of issue here. Since I don't know if the
    ;; Register Report properly ignores voided transactions, I'll err
    ;; on the side of safety by excluding them from the query....
    (qof-query-set-book query (gnc-get-current-book))
    (xaccQueryAddClearedMatch
      query (logand CLEARED-ALL (lognot CLEARED-VOIDED)) QOF-QUERY-AND)

    (qof-query-set-sort-order query
      (list SPLIT-TRANS TRANS-DATE-POSTED)
      (list QUERY-DEFAULT-SORT)
      '())
    (qof-query-set-sort-increasing query #t #t #t)

    (xaccQueryAddAccountMatch
      query
      (gnc-account-get-descendants-sorted
        (gnc-book-get-template-root (gnc-get-current-book)))
      QOF-GUID-MATCH-NONE
      QOF-QUERY-AND)

    (xaccQueryAddDateMatchTT query #t
      (gnc:time64-start-day-time
        (gnc:date-option-absolute-time
          (get-option gnc:pagename-general optname-from-date)))
      #t
      (gnc:time64-end-day-time
        (gnc:date-option-absolute-time
          (get-option gnc:pagename-general optname-to-date)))
      QOF-QUERY-AND)

    (gnc-set-option
      (gnc:optiondb (gnc:report-options report-obj)) "__reg" "query" (gnc-query2scm query))
    ;; we'll leave query malloc'd in case this is required by the C side...

    ;; just delegate rendering to the Register Report renderer...
    (renderer report-obj)))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "7936481ffcb34caeb24aa02e99754e33"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator general-journal-term-options-generator
 'renderer general-journal-term-renderer
 )

;; END
