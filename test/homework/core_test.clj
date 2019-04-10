(ns homework.core-test
  (:require [clojure.test :refer :all]
            [homework.core :refer :all]))

(deftest number-parsing
  (testing "Can read well formed numbers"
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "| || || || || || || || || |"
                 "|_||_||_||_||_||_||_||_||_|"
                 "                           "])
         "000000000"))
    (is (=
         (parse ["                           "
                 "  |  |  |  |  |  |  |  |  |"
                 "  |  |  |  |  |  |  |  |  |"
                 "                           "])
         "111111111"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 " _| _| _| _| _| _| _| _| _|"
                 "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                 ""])
         "222222222"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 " _| _| _| _| _| _| _| _| _|"
                 " _| _| _| _| _| _| _| _| _|"
                 ""])
         "333333333"))
    (is (=
         (parse ["                           "
                 "|_||_||_||_||_||_||_||_||_|"
                 "  |  |  |  |  |  |  |  |  |"
                 ""])
         "444444444"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                 " _| _| _| _| _| _| _| _| _|"
                 ""])
         "555555555"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                 "|_||_||_||_||_||_||_||_||_|"
                 ""])
         "666666666"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "  |  |  |  |  |  |  |  |  |"
                 "  |  |  |  |  |  |  |  |  |"
                 ""])
         "777777777"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "|_||_||_||_||_||_||_||_||_|"
                 "|_||_||_||_||_||_||_||_||_|"
                 ""])
         "888888888"))
    (is (=
         (parse [" _  _  _  _  _  _  _  _  _ "
                 "|_||_||_||_||_||_||_||_||_|"
                 " _| _| _| _| _| _| _| _| _|"
                 ""])
         "999999999"))
    (is (=
         (parse ["    _  _     _  _  _  _  _ "
                 "  | _| _||_||_ |_   ||_||_|"
                 "  ||_  _|  | _||_|  ||_| _|"
                 ""])
         "123456789")))
  (testing "Replaces malformed numbers with ?"
    (is (=
         (parse ["    _  _  _  _  _  _     _ "
                 "|_||_|| || ||_   |  |  | _ "
                 "  | _||_||_||_|  |  |  | _|"
                 ""])
         "49006771?"))
    (is (=
         (parse ["    _  _     _  _  _  _  _ "
                 "  | _| _||_| _ |_   ||_||_|"
                 "  ||_  _|  | _||_|  ||_| _ "
                 ""])
         "1234?678?"))))

(deftest checksum
  (testing "Returns true for numbers with correct checksums"
    (is (true? (valid-checksum? "000000000")))
    (is (true? (valid-checksum? "546424688")))
    (is (true? (valid-checksum? "468432468"))))
  (testing "Resturns true for numbers with incorrect checksums"
    (is (false? (valid-checksum? "111111111")))
    (is (false? (valid-checksum? "464684266")))
    (is (false? (valid-checksum? "684764235")))))

(deftest error-reporting
  (testing "Appends ERR for invalid checksum"
    (is (=
         (validate "684764235")
         "684764235 ERR"))
    (is (=
         (validate "111111111")
         "111111111 ERR")))
  (testing "Appends ILL for malformed numbers"
    (is (=
         (validate "269??8486")
         "269??8486 ILL"))
    (is (=
         (validate "?????????")
         "????????? ILL"))
    (is (=
         (validate "68432?545")
         "68432?545 ILL")))
  (testing "Return original number when valid"
    (is (=
         (validate "000000000")
         "000000000"))
    (is (=
         (validate "546424688")
         "546424688"))
    (is (=
         (validate "468432468")
         "468432468"))))
