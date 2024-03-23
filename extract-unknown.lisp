;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(init-transducers :kat :plain-og t)

#+test
(print (lookup-morphology :kat "გავთვალე" :variant :ng :lookup-guessed nil))

(defparameter *words* (make-hash-table :test #'equal))

(u:with-file-lines (line "projects:gnc;data;abuserisdze;text-1-5.txt")
  (dolist (word (u:split line #\space))
    (let ((word (delete-if (lambda (c)
                             (find c ".,:;?!„“()[1234567890]abcdefghijklmnopqrstuvwxyz	"))
                           word)))
      (when (> (length word) 0)
        (incf (gethash word *words* 0))))))

;; abuserisdze

;; 11011 / 980: 0.91 type recall
#+test
(let ((word-list
       (u:collecting
         (maphash (lambda (word count)
                    (u:collect (list word count)))
                  *words*))))
  (setf word-list (sort word-list #'> :key #'cadr))
  (print (reduce #'+ word-list :key #'cadr :initial-value 0))
  (terpri)
  (loop with sum = 0
        for (word count) in word-list
        for morph = (lookup-morphology :kat word :variety :og :lookup-guessed nil)
        for ng-morph = (unless morph (lookup-morphology :kat word :lookup-guessed nil))
        unless morph
        do (format t "~a	~a	~a~%" count word
                   (cond (morph "og") (ng-morph "ng") (t "-")))
        (incf sum count)
        finally (debug sum)))


#| unknown words:

980

27	დაჰმეტდეს
+25	მისსა
+19	დღისსაძიებელი
+19	წელიწდისა
18	ცხრამეტეულად
+?14	დაათუალე
+12	ამისსა
9	ოცდარვეულად
8	დაჰმეტდებოდეს
+8	წელიწდისაჲ
6	ამა
+5	ქრონიკონსა
5	შეიქნების
+5	მისისა
5	დაჰმეტდებიან
+?5	დაათუალვენ
5	დაჰმეტდების
+4	სეკდენბერსა
+4	ცხრამეტეულისა
4	წლითი
4	რაჲზომიცა
+4	ექუსეული
+4	ოცდარვეულისა
4	ქრისტესასა
4	ღამითგანსა
+4	წელიწდისად
+?4	დაათუალავ
4	დასაბამობითგანთა
4	ადიდებდითსა
4	რაჲზომსაცა
3	უგალობდითსა
+3	წელიწდითა
3	აიხუემდი
3	ძებნე
3	ვითარცა-რაჲ
3	განძლიერდასა
3	მისთასა
3	მათსა
3	კჳკლოსი
3	დაიმეტებს
3	დღისსაძიებელნი
3	მათისა
3	ოთხმეოცდაათექუსმეტითა
3	უკუღმა
3	ამისისა
3	მასწავებელი
3	ზე-უკუვარდების
3	დღისსაძიებელსა
3	დღისსაძიებელისა
3	ჰპოებენ
3	ცხრამეტეულსა
3	ევტუქი
3	ერთრიცხუად
2	ბოლოკ-ბასილი
2	სეკდენბერი
2	არტემონ
2	ტატიანე
2	მუნტანი
2	კჳკლოსისა
2	უოცოჲ
2	შეიგებ
2	კიკლოსი
2	ჰქჳან
2	ცხრამეტჯერ
2	უგანცხადებულესადრე
2	გულისჴმა-იყოფების
2	ათერთმეტეულებსა
2	მიჩუენებითა
2	წელიწდისასა
2	ანტიოქელი
2	დღისსაძიებელად
2	მარკიონ
2	ცამეტურისა
2	გარდაჴდებოდენ
2	უხრწნელო
2	აზმნობდეს
2	თხუთმეტი
2	ეთრად
2	რაჲზომიცაღა
2	წაიცვალებენ
2	აღგიარებთ
2	ცხრამეტეულნი
2	ღაღადყავსა
2	უწოდდის
2	ბ
2	ცხრამეტეული
2	ონორი
2	ისავრი
2	ერთგუამოვნებით
2	დაათუალევდი
2	თქუენისა
2	მისცემდის
2	ქრონიკონსაცა
2	ოქროსახელო
2	კაცისმშობელად
2	თექუსმეტსა
2	აკურთხევდითსა
2	დაესრულების
2	შეიგების
2	მარკელლე
2	განუტეობენ
2	სევეროზ
1	სრულ-ქმენ
1	მუნადმი
1	ზედადადებითა
1	ოკ-ლას
1	სიტჳსასა
1	ნუცაღა-რას
1	ივბიმიანე
1	მბრძოლობდეს
1	აპოლინარი
1	ემპოდოკლოჲსგან
1	სამნათელთა
1	ევვიონ
1	თევდოსი
1	ზედადასათუალავად
1	სძნობენ
1	უხრწნელსა
1	თანაწარვჰჴედ
1	სჳნმახოს
1	ათერთმეტეულებისანი
1	უწოდდა
1	ევდუქსი
1	კლავდის
1	ცრუწინაჲსწარმეტყუელნი
1	ჩასთუალვიდი
1	იძიების
1	ოქrრო-ბრწყინვალეთა
1	სევვატოს
1	განჴორციელუბული
1	ერისაკაცთაგანსაცა
1	ათცხრამეტობაჲ
1	ოცდარვეულნი
1	ყველიერობდი
1	შეუთქს
1	მიეძღუნებოდედ
1	იყუნენ
1	პრისკილა
1	თექუსმეტი
1	გულისჴმა-იყოფებოდის
1	ფებერვალvსა
1	აღრიცხუვითა
1	ნუუკუედა
1	მარტ-ლას
1	აღიარებოდედ
1	ძმაცუვიდა
1	დამიანოს
1	ზაქარწმიდაჲ
1	ერთგუამოვნებითისათჳს
1	გამოკლთოლვილად
1	ერთგუამოვნად
1	მზე-მთოვარებრმან
1	სჯულ-მცნებათა
1	მანინ
1	ნისტორის
1	უხრწნელმან
1	დაჰმეტდებდეს
1	თფილისისასა
1	კჳკლოსისათა
1	ათექუსმეტნი
1	მეყავნ
1	სათუალაობისა
1	დასაბამობითგანი
1	უვალენტიანონ
1	აღმოჰკაფდე
1	უოცოდ
1	სტრომატელი
1	ელკესიოს
1	დადოჲ
1	ნივთისსა
1	გარდმოტანებასა
1	მისთუალევდი
1	ათექუსმეტთა
1	დეკენბერსაჲ
1	ოთხმეოცდაrათექუსმეტთა
1	ჩემმიერი
1	ზედადათუალვითა
1	არმჯერნი
1	დამბადებლობისაებრცა
1	ევსტატი
1	ოცდარვაჯერ
1	ევნომი
1	ღმრთისსა
1	აეტი
1	ვთულიდეთ
1	ჯავთაგად
1	ტიმოთ
1	ჩაათუალე
1	ოცდარვეულსა
1	მისისანი
1	იან-ლას
1	მიხუმილნი
1	ცხოველობითითა
1	საცოდინელი
1	ნიავსულნელად
1	შემდგომითი-შემდგომად
1	ხუთეულ-ექუსეrულთათჳს
1	სამებისსა
1	შეძინებულისა
1	შემოიგდებს
1	მიჰმატენ
1	ცნობისსა
1	სევეროზ-ივლიანე
1	მიაჩუენებ
1	იამბიკონები
1	მისდად
1	მიუტევებდის
1	მიწყებით
1	ჴელთმქონებლობისაჲთა
1	კჳროს
1	უცოდინელსა
1	მოჰმეტდეს
1	არებდი
1	კჳროზ
1	მომპოვნნი
1	სევასტიელი
1	ცამეტურითა
1	ღირს-მიჩინე
1	ჯერ-მიჩნდა
1	გამოწულილვითრე
1	მიხუმილთა
1	მიძევია
1	კერდონ
1	დაათულიან
1	აკრიბის
1	მულიტი
1	განვიოტებთ
1	გუამ-ექმნა
1	კარპუკრატი
1	ვერსაცნობელ
1	ამათისა
1	შეჰvკაზმა
1	რაჲზომ
1	კჳრიაჲთურთ
1	თჳთოეულებამან
1	იჴუმია
1	აჰკრებდი
1	ღმრთივცემულთა
1	მომფსუესტელ
1	ორიგენი
1	ახოვანისაჲrთ
1	კაცყოფისა
1	ჯალე
1	ოცდათერთმეტეულსა
1	ოქრონათელსა
1	მამადმთავართა
1	ვოდდაvნტოს
1	შეაცილვებს
1	კჳკლოსისაჲ
1	რაჲზომისაჲცა
1	ევსევი
1	ღმრთივჴმოვნისა
1	ამათსაცა
1	წიაღსავლელი
1	ვალენტიოს
1	ბარსანოფი
1	ექუსეულად
1	რაჲზომისა
1	წააცვალებიოს
1	დღისსაძიებელთათჳს
1	ასტერიპპოს
1	ორღართასა
1	მომთხოველსა
1	მყოფისაებრ
1	კლიმი
1	ქრონიკონთაჲ
1	იპყრობებოდეს
1	კეთილ-ბოროტსა
1	ცხრამეტეულისათა
1	ოცდაათეულობასა
1	შევრაცხავთ
1	პოლემონ
1	გrუამსა
1	მესალიანად
1	ანასტასი
1	კოსტანტის
1	სამოსატელი
1	მძაგნი
1	დეკ-ლას
1	კოსტანტინეპოლისასა
1	შთამოვსრულდეთ
1	მეთურამეტესა
1	ლალუდი
1	არიოჲს
1	უწოდ
1	მისისასა
1	აღარაჲ
1	უკლებადრე
1	წელიწდისანი
1	ერთგუამოვნებამან
1	წაცვალოს
1	სიღრმეთაჲთაგან
1	მაvმათა
1	თორმეტჯერ
1	დღითი-დღე
1	შემნივთებელი
1	უკნინესობაჲ
1	წაvრმართთასა
1	ჭარალისაჲ
1	ნაცვლად
1	სულისსა
1	სთხოის
1	დაჰმეrტდეს
1	ერთობ
1	დღისსაძიrებელნი
1	აღრიცხავ
1	განგებითა-იამბიკონითა
1	კეთილ-ბოროტთა
1	ღირს-იქმნა
1	ღმრთივცემული
1	ჩუქვების
1	აღვიარებდეთ
1	წაიცვალებს
1	სატორნილოს
1	გქადაგეს
1	ღმრთივბრწყინვაvლისა
1	ქრისტესათა
1	ორგუამად
1	გარrდაჴდა
1	ოცდა-რაოდენიცა
1	ალექსანდრელი
1	მტირ
1	ქურდაობით
1	ბძანებული
1	მიაჩუენებენ
1	ჴელყვის
1	იოხდი
1	ადელფოჲ
1	კირინთისს
1	აღმომიკაზმავს
1	დღისსაძიებლითა
1	მეტყუელ-იქმნნეს
1	ღაღატყავსა
1	დღითად
1	იოვანეთანი
1	ღმრთივბრწყინვალისა
1	მიათულიდით
1	გეპოვნების
1	თეოდოტონ
1	სიტჳსა
1	იჩქურა
1	გამოიძივნე
1	ოთხმეოცდათხუთმეტითა
1	თხუთმეტსა
1	თეოდოტოს
1	ორმეოცდააცამეტი
1	იამვოთასა
1	კჳკლო-სისაჲ
1	ტჳრელი
1	წამოსთუალვენ
1	წასცვალებს
1	ადგილითი
1	ღმრთივცურეულად
1	მანენდროს
1	თანაარსებაჲ
1	ვიარებით
1	ფილასოფოსობისაჲ
1	ვსცნობთ
1	ფებ-კან
1	თურამეტი
1	დაითრგუნა
1	სამოსატელისაჲ
1	დარჩებოდენ
1	არიანოზთა
1	ოცდაათჩუიდმეტი
1	მაქსიმოს
1	ზეუკუ
1	სამმწყობრთა
1	დღისსაძიებელითურთ
1	ეvსევითარითა
1	დაიყენების
1	ცამეტური
1	აიხუმიდი
1	განმაფრთხვე
1	გაგისაჭიროვდების
1	ათჯერ
1	ზედნადეvბი
1	მოქმედებული
1	ჰბრძვე
1	მანიქეოს
1	შეიგებიან
1	აღ-რაჲ-ადგინეს
1	უთქს
1	დღისსაძიებელისათა
1	პრასინაული
1	გამოთrქუმაჲ
1	ქების-ვმეტყუელებ
1	ანდრიანეს
1	ექუrსსა
1	პირუტყუ-ქმნული
1	ხრწნილებასა
1	ახოვაrნი
1	უპირატეთა
1	სამგუამოვნად
1	მიაჩუენებს
1	პირველდამწყებელნი
1	აღიტჳრთე
1	იბ-ნი
1	განსაწყინოვდების
1	ჰნათობს
1	გაიანოს
1	საrწყინოობისათჳს
1	აიჴოცოს
1	ამისსავე
1	ზედაწარწერილსა
1	ჩასთულიდი
1	დეკეოჲს
1	იანბიკონთაჲთა
1	წინაწარმოძღომილმან
1	ტარსელი
1	ღმრთივაღმომდინარედ
1	დააკლე
1	დაჰმეტდებოდის
1	ნათლისმღებელნი
1	შეისწავეთა
1	ექუსეvულებად
1	კაცისშვილთასა
1	ცამეტურით
1	ბოლოკ-ბასილობით
1	ვარდისან
1	სევეროზის
1	აrნუ
1	სეკდენბერისაჲ
1	ნიკოლაოზ
1	შეიგებთ
1	გარდმოიხრწნა
1	მოგითხრა
1	მზღაპრობი
1	კუანილი
1	ბოროტმგმობარი
1	ოცდარაჲზომსაცა
1	განაღმრთვნა
1	აღრაცხეს
1	სიტყუაქმნასა
1	ღალინო
1	ცამეტურნი
1	პოლიმონ
1	გიხაროდენი
1	კაპპატონსა
1	ნისტორ
1	თანაწარვჰჴე
1	ჰნათობენ
1	იყrო
1	მიუწოდოს
1	აღჰრაvცხავს
1	სავვატ
1	მიჰმატე
1	ვერხილვასა
1	ოთხმეოცდაათექუსმეტნი
1	მისისაჲ
1	ქუემოჲთრე
1	სამებისგანად
1	განაძლიერებდის
1	თურამეტამდე
1	უხრწნელისა
1	ა
1	დ
1	შუარტყალს
1	მაის-ლას
1	უდღისსაძიებელობს
1	შეიქნებოდეს
1	ონორის
1	ღმრთივმოძრავად
1	ზედაწარ-წერილსა
1	ჯერ-იჩინა
1	ჩაისწავლე
1	ტერევნითად
1	ღმრთისგანად
1	მო-რაჲ-იწია
1	ათექუსმეტი
1	ზედანადებსაცა
1	დღისსაძიებელიცა
1	და-ცა-იმეტნეს
1	ივლიანოს
1	ვმსახურებთ
1	არც
1	აღმობრწყინვებულად
1	მოჲწია
1	ოთხმეოცდათექუსმეტთა
1	კურთხეულარსა
1	ამათსა
1	განმრავლდებოდენ
1	ერთგუამოვნებად
1	კაცქმნილი
1	წულილადრე
1	აგრილობდა
1	ნონ-ლას
1	ექუსეvულთაჲ
1	განგემარჯუებოდეს
1	დაჰმატებდი
1	აცხრომებს
1	თეოდოტი
1	მომფსუესტელისა
1	იპოვნების
1	გუამითი-გუამად
1	მოლოდებითთა
1	მეთხუთმეტისა
1	სხუანიც
1	მივანდევ
1	პირველ-პირველობითად
1	კჳრიაკეობისათჳს
1	განასაზღვრებს
1	მეთხუთმეტესაცა
1	მანენტოჲს
1	მგონებლობაჲ
1	მოყავ
1	კლარჯეთისსა
1	ღასტი
1	ერთეკლესიად
1	პონილი
1	კეთილ-დასაბამისა
1	დასასრულ-დასაბამი
1	ალიკარნელი
1	გქონდენ
1	მონოზონებასა
1	დაათუალენი
1	ღმრთისმბრძოლო
1	ივნის-ლას
1	დღისით
1	მოჰმედ
1	შეიქნებიან
1	თეvქუსმეტსა
1	სახელიდების
1	თჳთებაყოფად
1	მეთექუსმეტისა
1	მრჩობლშეკეცათა
1	აკმაა
1	ოქროელვასა
1	ოცდარვეული
1	ეvსრეთვე
1	მოივლინა
1	დაბადებითგანნი
1	მიმწთომად
1	ვეროცქმნილი
1	ერთხელ
1	ზემოიჲთწერილისანი
1	დღისსაძიებელთათჳსვე
1	წინაჲსთა
1	ენა-ბრგუნვილად
1	ბაჩილო
1	კჳკლოსსა
1	წლითიწლადობით
1	სიტჳსაებრ
1	აკრბეს
1	ჴელთგანპყრობით
1	აღდგომისგანთა
1	გუამოვნებითისა
1	სეკ-ლას
1	შემომვედრა
1	ვცოცხლებდე
1	ბოლოკ-ბასილის
1	იამბიკონად
1	სჳნეპოს
1	ჟამთამი
1	აღვსებითგანი
1	ფარანელი
1	მიათულიდი
1	თურამეტსა
1	IXII
1	ეზოჲსმოძღურად
1	თხოის
1	ენთჳსიასტად
1	ბეrრძულად
1	მაქსიმილა
1	სამოსატელისა
1	ვიიძულე
1	აპ-ლას
1	დათულითა
1	სახისშემატებისაებრ
1	მელნისამიერი
1	მოიჴმარებიან
1	საწყინო
1	მათისაებრ
1	იამბიკონთა
1	დასაქებელ
1	ოქრომდინარედ
1	მივყავ
1	ღმრთივმღელვარედ
1	უვარმქნელნი
1	ოთხმეოცდამეჩუიდმეტესა
1	უხრწნელად
1	შეიგებდე
1	სკჳთანესაგან
1	შეიგებდი
1	განიტევებოდეს
1	წელიწდად
1	დღისსაძიებელთასა
1	ნუცარას
1	შეგაგებიებს
1	დაათუალენ
1	დღისსაძიებლითურთ
1	ძმაცუვიდეს
1	ევჰიტთ
1	ივლის-აგჳსტოს
1	ღირს-იჩინა
1	გუამოვნებულისა
1	მესმაასა
1	ჴორცთშესხმასა
1	ღმრთივბრწყინვალე
1	გეწყინებოდეს
1	ხუთეულ-ექუსეულითურთ

|#


;; Tinatin

;; 
#+test
(with-open-file
    (stream
     ;; "projects:georgian-morph;wordlists;tinatin-oov-analyzed.txt" ;; (:COUNT 843.890 :FOUND 159.983)
     ;; "projects:georgian-morph;wordlists;ngo-new-analyzed.txt" ;; (:COUNT 21576 :FOUND 7717) 36%
     "projects:georgian-morph;wordlists;gazetebi-analyzed.txt" ;; (:COUNT 66145 :FOUND 28998) 44%
     :direction :output :if-exists :supersede)
  (let ((count 0)
        (found-count 0))
    (u:with-file-lines (word ;; "projects:georgian-morph;wordlists;tinatin-oov.txt"
                        ;; "projects:georgian-morph;wordlists;ngo-new.txt"
                        "projects:georgian-morph;wordlists;gazetebi.txt"
                        )
      (let ((morph (lookup-morphology :kat word :lookup-guessed nil)))
        (when morph (incf found-count))
        (incf count)
        (if morph
            (format stream "~a	~{~a~^ | ~}~%" word
                    (remove-duplicates
                     (mapcar (lambda (r)
                               (let ((lemma (car r))
                                     (morph (delete-if-not
                                             (lambda (m)
                                               (find m '("N" "Prop" "Top" "Hydr" "V" "Act" "Prop" "Anthr"
                                                         "LastName" "FirstName" "Pron" "Punct" "A"
                                                         "Adv" "Cj" "Compl" "PP" "Interj" "Punct")
                                                     :test #'string=))
                                             (u:split (cadr r) #\space))))
                                 (format nil "~a ~{~a~^+~}" lemma morph)))
                             morph)
                     :test #'equal))
            (write-line word stream)
            )))
    (print (list :count count :found found-count))))

#+test
(print (lookup-morphology :kat "არაინერციული" :lookup-guessed nil))

#+test
(parse-kat-file "projects:gnc;data;tina;corp_dict_ge-moambe-ka-202402229.csv"
                :format :tsv :write-xml-id nil)

#+test
(with-open-file (stream "projects:gnc;data;tina;corp_dict_ge-moambe-ka-202402229-guessed.tsv"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:gnc;data;tina;corp_dict_ge-moambe-ka-202402229.tsv") 
    (when (search "Guess" line)
      (write-line line stream))))

(defparameter *word-list* (dat:make-string-tree))

#+test
(let ((word-list *word-list*))
  (setf word-list (dat:make-string-tree))
  (with-open-file (stream "projects:gnc;data;tina;corp_dict_ge-moambe-ka-202402229-guessed.tsv"
                          :direction :output :if-exists :supersede)
    (u:with-file-lines (line "projects:gnc;data;tina;corp_dict_ge-moambe-ka-202402229.tsv") 
      (when (search "Guess" line)
        (destructuring-bind (word lemmas features-list)
            (u:split line #\tab)
          (loop for lemma in (u:split lemmas #\¦ nil nil t)
                and features in (u:split features-list #\¦ nil nil t)
                for lemma-words = (dat:string-tree-get word-list lemma)
                do (when (null lemma-words)
                     (setf lemma-words (dat:make-string-tree)))
                (incf (dat:string-tree-get
                       lemma-words
                       (u:concat word (subseq features (1+ (or (position #\> features) -1))))
                       0))
                (setf (dat:string-tree-get word-list lemma) lemma-words)))
        ;;(write-line line stream)
        ))
    (dat:do-string-tree (lemma lemma-words word-list)
      (let ((lcount 0))
        (dat:do-string-tree (lemma+features count lemma-words)
          (incf lcount count))
        (format stream "~a	~a~%" lcount lemma)
        (dat:do-string-tree (lemma+features count lemma-words)
          (format stream "	~a	~a~%" count lemma+features)))
    )))


#+test
(with-open-file (stream "projects:gnc;data;tina;names-first-unknown.txt"
                        :direction :output :if-exists :supersede)
  (dolist (gend '("fem" "masc"))
    (u:with-file-lines (word (format nil "projects:gnc;data;tina;names-first-~a.txt" gend))
      (let ((morph (lookup-morphology :kat word :lookup-guessed nil)))
        (unless (find-if (lambda (r) (equal (cadr r) "N Prop Anthr FirstName Nom")) morph)
          (write-line word stream))))))

#+test
(parse-kat-file "projects:gnc;data;tina;names-pers.txt"
                :format :tsv :write-xml-id nil)


#+test
(with-open-file (stream "projects:gnc;data;tina;names-geo-unknown.txt"
                        :direction :output :if-exists :supersede)
  (let ((unknown ()))
    (u:with-file-lines (word "projects:gnc;data;tina;names-geo.txt")
      (let ((morph (lookup-morphology :kat word :lookup-guessed nil)))
        (unless (find-if (lambda (r) (search "N Prop Top" (cadr r))) morph)
          (pushnew word unknown :test #'equal ))))
    (setf unknown (sort unknown (lambda (a b) (string< (reverse a) (reverse b)))))
    (dolist (word unknown)
      (write-line word stream))))

:eof
