(require 'ert)
(require 'buttercup)
(require 'radio-utils)

(describe "radio--valid-grid-p"
  (it "should return valid for"
    (expect (radio--valid-grid-p "CN") :to-be-truthy)
    (expect (radio--valid-grid-p "CN87") :to-be-truthy)
    (expect (radio--valid-grid-p "CN87TQ") :to-be-truthy))
  (it "should return invalid for"
    (expect (radio--valid-grid-p "CS") :not :to-be-truthy)
    (expect (radio--valid-grid-p "CN87TY") :not :to-be-truthy)
    (expect (radio--valid-grid-p "CNTQ") :not :to-be-truthy)
    (expect (radio--valid-grid-p "87") :not :to-be-truthy)
    (expect (radio--valid-grid-p "CN97TQAB") :not :to-be-truthy)))

(describe "Maidenhead to latitude & longitude"
  (it "Should work for a field:"
    (expect (radio-grid-to-latlong "CN" ) :to-equal '(40 . -140)))
  (it "Should calculate for field and square"
    (expect (radio-grid-to-latlong "CN87") :to-equal '(47 . -124)))
  (it "should calculate down to subsquare"
    (let ((result (radio-grid-to-latlong "CN87TQ")))
      (expect (car result) :to-be-close-to 47.6666666666 10)
      (expect (cdr result) :to-be-close-to -122.41666666666 10)))
  (it "should error with invalid grid reference"
    (expect (radio-grid-to-latlong "SS") :to-throw)))

(describe "Latitude and longitude to Maidenhead"
  (it "Should return the correct field CN87TQ"
    (expect (radio-latlong-to-grid '(47.706 . -122.409)) :to-equal "CN87TQ"))
  (it "Should return the correct field KQ20CB"
    (expect (radio-latlong-to-grid '(70.0639336 . 24.18888690)) :to-equal "KQ20CB"))
  (it "Should return the correct field PF95TR"
    (expect (radio-latlong-to-grid '(-34.2595 . 139.626)) :to-equal "PF95TR"))
  (it "Should return the correct field FB71DC"
    (expect (radio-latlong-to-grid '(-77.8938405 . -65.72564)) :to-equal "FB72DC")))


(ert-deftest grid-char-conversion ()
  (should (equal (radio--grid-to-ord ?A) 0))
  (should (equal (radio--grid-to-ord ?X) 23))
  (should (equal (radio--grid-to-ord ?0) 0))
  (should (equal (radio--grid-to-ord ?4) 4)))
