#lang racket/gui

(require data-frame)
(require racket/gui/base)
(require plot)
;plot the graphs in a new window
(plot-new-window? #t)


;Aux function so that days start counting from 1
(define (add_money_days list_days days)
  (add_money_days_exe list_days days 1)
)

;This function iterates until it reaches all the days that will appear in the GUI, for each iteration it creates a new text-field and adds it to a list
;Once we are done with all the days, the list that contains all the texx-fields is returned.
(define (add_money_days_exe list_days days aux)
  (cond
    [(> aux (- days 1)) list_days]
    [else (add_money_days_exe (cons (new text-field% [parent frame][label (string-append "Day " (~v aux))]) list_days) days (+ aux 1))]
  )
)

;Rackets GUI receives list to plot them, so this function builds a list of days to plot in the x axis which will represent the days that have passes so far.
(define (build_x_axis list days)
  (cond
    [(= days 0) list]
    [else (build_x_axis (cons days list) (- days 1))]
  )
)

;This function iterates the text field list, and for each field it checks its value, if the value is not empty that means that it contains a numeric value
;This value is added to the total acum and to the list that contains all the acums for each day. If the value in the field is an empty string then the list
;just adds a -50, because we are going to plot from Y = 0, so any negative value will not appear and that is exactly what we want
(define (build_money_spent_acum list text_field_list acum)
  (cond
    [(empty? text_field_list) (reverse list)]
    [(equal? "" (send (first text_field_list) get-value)) (build_money_spent_acum (cons -50 list) (rest text_field_list) acum)]
    [else (build_money_spent_acum (cons (+ acum (string->number (send (first text_field_list) get-value))) list) (rest text_field_list) (+ acum (string->number (send (first text_field_list) get-value))))]
  )
)

;Aux function that initialices the day count and the money acum in 0
(define (get_info money_acum_list)
  (get_info_exe money_acum_list 0 0)
)

;This function will iterate the list that contains the acum of money for each day until it finds a negative value (empty text field) or ends iterating the whole list
;When one of those 2 conditions is met, the function will return a list of 2 elements: the total days that passed and the total money acum until that day
(define (get_info_exe money_acum_list days_count money_acum)
  (cond
    [(empty? money_acum_list) (cons days_count (cons money_acum empty))]
    [(< (first money_acum_list) 0) (cons days_count (cons money_acum empty))]
    [else (get_info_exe (rest money_acum_list) (+ days_count 1) (first money_acum_list))]
  )
)

;This function opens a file and returns a data-frame with the file's content
(define (open_csv filename)
  (df-read/csv filename)
)

;to load info first we open the money.txt file and store it, the first series of all the series contained in the file will contain
;all the values that were saved, separated by a space, so now we split that string into a list and make the call to the load_info_exe funcion
;sending the info that we loaded and the list of text field objects that are drawn in the GUI.
(define (load_info text_list)
  (define df (open_csv "money.txt"))
  (define loaded_data (string-split(first (df-series-names df))))
  (load_info_exe loaded_data text_list)
)


;For all the info that is in the info_list, we are going to set the value of the corresponding text field
;We repeat this process until there is no info left because almost all of the times not all the text fields
;will have values when the save button is pressed
(define (load_info_exe info_list text_list)
  (cond
    [(empty? info_list) text_list]
    [else (begin
            (send (first text_list) set-value (first info_list))
            (load_info_exe (rest info_list) (rest text_list))
          )]
  )
)

;This function iterates over all the text fields and checks their value, if they have something then the value is added to the string list
;This functions is basically used to prepare the data to be saved in the file, it is way easier to have it in a list of strings than to have
;to go object by object and get their field values
(define (get_money_days_string string_list text_field_list)
  (cond
    [(equal? "" (send (first text_field_list) get-value)) string_list]
    [else (get_money_days_string (cons (send (first text_field_list) get-value) string_list) (rest text_field_list))]
  )
)

;To save the info we create a new data frame, and then we get a list of strings that contain all the values that are currently stored in
;all the text fields, then we join all the strings separated by spaces into a string and turn it again to a list (it will only contain 1 element)
;this list is turned into a vector because series can only receive data in form of vectors. We did not have to join the strings but this way
;they will all appear in just one line of the file which makes it way easier to load them.
;We add the series to the data frame and create the file money.txt with the information inside the data frame
(define (save_info text_list)
  (define new_df (make-data-frame))
  (define new_data_list (list (string-join (reverse (get_money_days_string '() text_list)) " ")))
  (define new_data_vector (list->vector new_data_list))
  (define new_data (make-series (first new_data_list) #:data new_data_vector))
  (df-add-series new_df new_data)
  (df-write/csv new_df "money.txt")
)

(define total_days 15)

; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Project"]
                   [width 500]
                   [height 800]
                   ))

; Make a text field instantiating the text-field% class
(define money_total (new text-field%
                         [parent frame]
                         [label (string-append "Total money earned in " (~v total_days) " days")]

                         ))

;we define a variable that creates a text field for each day
(define money_days (add_money_days '() total_days))
;Build the x axis list that will be plotted
(define x_days (build_x_axis '() (- total_days 1)))
;(define info_msg (new message% [parent frame] [label "prueba"]))

; Make a button for plotting instantiating the button% class
(define plot_btn(new button%
                     [parent frame]
                     [label "Plot"]
                     ;callback is the action that will be executed once the button is clicked
                     [callback (lambda (button event)
                                 ;We get the total money value that is in the text field
                                 (define money_total_value (string->number (send money_total get-value)))
                                 ;The average money that should be spent each day is = total money / total days
                                 (define avg_money_day (/ money_total_value total_days))
                                 ;We create a list that contains the money acum spent for each day
                                 (define money_spent_acum (build_money_spent_acum '() (reverse money_days) 0))
                                 (define info_list (get_info money_spent_acum))
                                 ;from get_info function we can get how many days have passed and how much money has been spent to make some simple calculations for feedback
                                 (define passed_days (first info_list))
                                 (define curr_spent (first (rest info_list)))
                                 ;Based on the days that are left and the money that is left we can calculate the new average money to spend in order to survive
                                 ;This will be equal to (total money - total money spent) / (total days - passed days)
                                 (define new_avg_money_day (floor (exact->inexact (/ (- money_total_value curr_spent) (- total_days passed_days)))))
                                 ;PLot the total money spent for each day in the y axis and the days in the x axis
                                 (plot (list
                                        (function (lambda (x) (* x avg_money_day)) #:label "Average")
                                        (points (map vector x_days money_spent_acum) #:label "Real")
                                       )
                                       #:x-min 0 #:x-max 15 #:y-min 0
                                 )
                                 ;; Make a message instantiating the message% class that displays the relevant information that we calculated before plotting
                                 (new message% [parent frame] [label (string-append "\nRELEVANT INFORMATION\nYou have spent a total amout of $" (~v curr_spent)
                                                                      " (pesos) in " (~v passed_days) " day(s).\nIf you want to survive, you can't spend more than\n$" (~v new_avg_money_day)
                                                                      " (pesos) a day for the next " (~v (- total_days passed_days)) " day(s).")])
                               )]
                 ))



; Make a button for loading data instantiating the button% class
(define load_btn(new button%
                     [parent frame]
                     [label "Load"]
                     [callback (lambda (button event)
                                 ;Once the button is pressed we call the laod_info function and send the list of all the text fields so we can open the file
                                 ;and set one by one each text field's value
                                 (load_info (cons money_total (reverse money_days)))
                               )]
                 ))

; Make a button for saving data instantiating the button% class
(define save_btn(new button%
                     [parent frame]
                     [label "Save"]
                     [callback (lambda (button event)
                                 ;Simliar to the load button, we will call a function and send the list of all text fields, but now it will be the save_info function that is
                                 ;in charge of getting the values of all of them and generate the series and then the data frame to add the information to a file
                                 (save_info (cons money_total (reverse money_days)))
                               )]
                 ))


; Show the frame by calling its show method
(send frame show #t)