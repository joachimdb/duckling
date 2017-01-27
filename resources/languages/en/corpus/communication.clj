(
 {}
 
 "650-283-4757"
 "+1 6502834757"
 "+33 4 76095663"
 "06 2070 2220"
 "(650)-283-4757 ext 897"
 (fn [_ token]
   (when-not (and (= :phone-number (:dim token))
                  (= (:text token) (:value token)))
     [{:dim :phone-number
       :value (:text token)
       :text (:value token)} token]))
 
 "http://www.bla.com"
 "www.bla.com:8080/path"
 "https://myserver?foo=bar"
 "cnn.com/info"
 "bla.com/path/path?ext=%23&foo=bla"
 "localhost"
 "localhost:8000"
 "http://kimchi" ; local url
 (fn [_ token]
   (when-not (and (= :url (:dim token))
                  (= (:text token) (:value token)))
     [{:dim :url
       :value (:text token)
       :text (:value token)} token]))
 
 "alex@wit.ai"
 "alex.lebrun@mail.wit.com"
 (fn [_ token]
   (when-not (and (= :email (:dim token))
                  (= (:text token) (:value token)))
     [{:dim :email
       :value (:text token)
       :text (:value token)} token]))
 
 )
