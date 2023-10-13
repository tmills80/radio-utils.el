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
    (expect (radio-grid-to-latlong "CN" ) :to-equal '(40 -140)))
  (it "Should calculate for field and square"
    (expect (radio-grid-to-latlong "CN87") :to-equal '(47 -124)))
  (it "should calculate down to subsquare"
    (let ((result (radio-grid-to-latlong "CN87TQ")))
      (expect (car result) :to-be-close-to 47.6666666666 10)
      (expect (cadr result) :to-be-close-to -122.41666666666 10)))
  (it "should error with invalid grid reference"
    (expect (radio-grid-to-latlong "SS") :to-throw)))



(ert-deftest grid-char-conversion ()
  (should (equal (radio--grid-to-ord ?A) 0))
  (should (equal (radio--grid-to-ord ?X) 23))
  (should (equal (radio--grid-to-ord ?0) 0))
  (should (equal (radio--grid-to-ord ?4) 4)))
