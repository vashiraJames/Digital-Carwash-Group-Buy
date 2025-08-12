;; group-buy.clar
;; Digital Carwash Group Buy System

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GROUP-NOT-FOUND (err u101))
(define-constant ERR-GROUP-FULL (err u102))
(define-constant ERR-GROUP-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-ALREADY-MEMBER (err u105))

(define-data-var next-group-id uint u1)

(define-map groups
  { group-id: uint }
  {
    creator: principal,
    service-type: (string-ascii 50),
    target-size: uint,
    current-size: uint,
    price-per-person: uint,
    deadline-block: uint,
    is-active: bool
  }
)

(define-map group-members
  { group-id: uint, member: principal }
  { joined-block: uint, paid: bool }
)

(define-map group-payments
  { group-id: uint }
  { total-collected: uint, withdrawn: bool }
)

(define-public (create-group (service-type (string-ascii 50)) (target-size uint) (price-per-person uint) (duration-blocks uint))
      (let
    ((group-id (var-get next-group-id))
     (deadline (+ burn-block-height duration-blocks)))
    (map-set groups
      { group-id: group-id }
      {
        creator: tx-sender,
        service-type: service-type,
        target-size: target-size,
        current-size: u0,
        price-per-person: price-per-person,
        deadline-block: deadline,
        is-active: true
      }
    )
    (map-set group-payments
      { group-id: group-id }
      { total-collected: u0, withdrawn: false }
    )
    (var-set next-group-id (+ group-id u1))
    (ok group-id)
  )
)

(define-public (join-group (group-id uint))
  (let
    ((group-data (unwrap! (map-get? groups { group-id: group-id }) ERR-GROUP-NOT-FOUND))
     (payment-data (unwrap! (map-get? group-payments { group-id: group-id }) ERR-GROUP-NOT-FOUND)))
    (asserts! (get is-active group-data) ERR-GROUP-CLOSED)
    (asserts! (< burn-block-height (get deadline-block group-data)) ERR-GROUP-CLOSED)
    (asserts! (< (get current-size group-data) (get target-size group-data)) ERR-GROUP-FULL)
    (asserts! (is-none (map-get? group-members { group-id: group-id, member: tx-sender })) ERR-ALREADY-MEMBER)

    (try! (stx-transfer? (get price-per-person group-data) tx-sender (as-contract tx-sender)))

    (map-set group-members
      { group-id: group-id, member: tx-sender }
      { joined-block: burn-block-height, paid: true }
    )
    (map-set groups
      { group-id: group-id }
      (merge group-data { current-size: (+ (get current-size group-data) u1) })
    )
    (map-set group-payments
      { group-id: group-id }
      (merge payment-data { total-collected: (+ (get total-collected payment-data) (get price-per-person group-data)) })
    )
    (ok true)
  )
)

(define-public (close-group (group-id uint))
  (let
    ((group-data (unwrap! (map-get? groups { group-id: group-id }) ERR-GROUP-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator group-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active group-data) ERR-GROUP-CLOSED)

    (map-set groups
      { group-id: group-id }
      (merge group-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (withdraw-payments (group-id uint))
  (let
    ((group-data (unwrap! (map-get? groups { group-id: group-id }) ERR-GROUP-NOT-FOUND))
     (payment-data (unwrap! (map-get? group-payments { group-id: group-id }) ERR-GROUP-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator group-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-active group-data)) ERR-GROUP-CLOSED)
    (asserts! (not (get withdrawn payment-data)) ERR-NOT-AUTHORIZED)

    (try! (as-contract (stx-transfer? (get total-collected payment-data) tx-sender (get creator group-data))))

    (map-set group-payments
      { group-id: group-id }
      (merge payment-data { withdrawn: true })
    )
    (ok true)
  )
)

(define-read-only (get-group (group-id uint))
  (map-get? groups { group-id: group-id })
)

(define-read-only (get-member-status (group-id uint) (member principal))
  (map-get? group-members { group-id: group-id, member: member })
)

(define-read-only (get-payment-status (group-id uint))
  (map-get? group-payments { group-id: group-id })
)
