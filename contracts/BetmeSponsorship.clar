;; BetmeSponsorship - Challenge sponsorship system
;; Allows third parties to sponsor challenges by adding prize money and rewards

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u300))
(define-constant err-challenge-not-found (err u301))
(define-constant err-invalid-amount (err u302))
(define-constant err-sponsorship-not-found (err u303))
(define-constant err-sponsorship-claimed (err u304))
(define-constant err-challenge-not-completed (err u305))
(define-constant err-insufficient-funds (err u306))
(define-constant err-sponsor-limit-reached (err u307))

;; Sponsorship data
(define-data-var next-sponsorship-id uint u1)
(define-data-var max-sponsors-per-challenge uint u10)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%

;; Track sponsorships for challenges
(define-map challenge-sponsorships
  uint ;; sponsorship-id
  {
    challenge-id: uint,
    sponsor: principal,
    sponsored-amount: uint,
    bonus-condition: (string-ascii 50), ;; "winner", "both", "completion"
    sponsor-message: (string-ascii 200),
    created-block: uint,
    is-claimed: bool,
    claim-block: (optional uint)
  }
)

;; Track total sponsorship per challenge
(define-map challenge-sponsor-totals
  uint ;; challenge-id
  {
    total-sponsored: uint,
    sponsor-count: uint,
    winner-bonus-pool: uint,
    completion-bonus-pool: uint
  }
)

;; Track sponsors for each challenge
(define-map challenge-sponsors
  { challenge-id: uint, sponsor: principal }
  {
    total-contributed: uint,
    sponsorship-count: uint,
    first-sponsorship-block: uint
  }
)

;; Sponsor leaderboard
(define-map sponsor-stats
  principal
  {
    total-sponsored: uint,
    challenges-sponsored: uint,
    successful-sponsorships: uint,
    total-bonuses-distributed: uint
  }
)

;; Add sponsorship to a challenge
(define-public (sponsor-challenge (challenge-id uint) (amount uint) (condition (string-ascii 50)) (message (string-ascii 200)))
  (let (
    (sponsorship-id (var-get next-sponsorship-id))
    (platform-fee (/ (* amount (var-get platform-fee-percentage)) u10000))
    (net-amount (- amount platform-fee))
    (current-totals (default-to 
      { total-sponsored: u0, sponsor-count: u0, winner-bonus-pool: u0, completion-bonus-pool: u0 }
      (map-get? challenge-sponsor-totals challenge-id)
    ))
    (current-sponsor-info (default-to
      { total-contributed: u0, sponsorship-count: u0, first-sponsorship-block: stacks-block-height }
      (map-get? challenge-sponsors { challenge-id: challenge-id, sponsor: tx-sender })
    ))
    (sponsor-stats-current (default-to
      { total-sponsored: u0, challenges-sponsored: u0, successful-sponsorships: u0, total-bonuses-distributed: u0 }
      (map-get? sponsor-stats tx-sender)
    ))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (< (get sponsor-count current-totals) (var-get max-sponsors-per-challenge)) err-sponsor-limit-reached)
    (asserts! (or (is-eq condition "winner") (is-eq condition "both") (is-eq condition "completion")) err-unauthorized)
    
    ;; Transfer sponsorship amount
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create sponsorship record
    (map-set challenge-sponsorships sponsorship-id
      {
        challenge-id: challenge-id,
        sponsor: tx-sender,
        sponsored-amount: net-amount,
        bonus-condition: condition,
        sponsor-message: message,
        created-block: stacks-block-height,
        is-claimed: false,
        claim-block: none
      }
    )
    
    ;; Update challenge totals based on condition
    (let (
      (new-winner-bonus (if (or (is-eq condition "winner") (is-eq condition "both"))
                           (+ (get winner-bonus-pool current-totals) net-amount)
                           (get winner-bonus-pool current-totals)))
      (new-completion-bonus (if (or (is-eq condition "completion") (is-eq condition "both"))
                              (+ (get completion-bonus-pool current-totals) net-amount)
                              (get completion-bonus-pool current-totals)))
    )
      (map-set challenge-sponsor-totals challenge-id
        {
          total-sponsored: (+ (get total-sponsored current-totals) net-amount),
          sponsor-count: (if (is-eq (get sponsorship-count current-sponsor-info) u0)
                            (+ (get sponsor-count current-totals) u1)
                            (get sponsor-count current-totals)),
          winner-bonus-pool: new-winner-bonus,
          completion-bonus-pool: new-completion-bonus
        }
      )
    )
    
    ;; Update sponsor info for this challenge
    (map-set challenge-sponsors { challenge-id: challenge-id, sponsor: tx-sender }
      {
        total-contributed: (+ (get total-contributed current-sponsor-info) net-amount),
        sponsorship-count: (+ (get sponsorship-count current-sponsor-info) u1),
        first-sponsorship-block: (get first-sponsorship-block current-sponsor-info)
      }
    )
    
    ;; Update sponsor global stats
    (map-set sponsor-stats tx-sender
      {
        total-sponsored: (+ (get total-sponsored sponsor-stats-current) net-amount),
        challenges-sponsored: (if (is-eq (get sponsorship-count current-sponsor-info) u0)
                                (+ (get challenges-sponsored sponsor-stats-current) u1)
                                (get challenges-sponsored sponsor-stats-current)),
        successful-sponsorships: (get successful-sponsorships sponsor-stats-current),
        total-bonuses-distributed: (get total-bonuses-distributed sponsor-stats-current)
      }
    )
    
    (var-set next-sponsorship-id (+ sponsorship-id u1))
    (ok sponsorship-id)
  )
)

;; Claim sponsorship rewards (for winners or completion)
(define-public (claim-sponsorship-bonus (challenge-id uint) (bonus-type (string-ascii 50)))
  (let (
    (totals (unwrap! (map-get? challenge-sponsor-totals challenge-id) err-challenge-not-found))
  )
    (asserts! (or (is-eq bonus-type "winner") (is-eq bonus-type "completion")) err-unauthorized)
    
    (let (
      (bonus-amount (if (is-eq bonus-type "winner")
                       (get winner-bonus-pool totals)
                       (get completion-bonus-pool totals)))
    )
      (asserts! (> bonus-amount u0) err-insufficient-funds)
      
      ;; Transfer bonus to claimer
      (try! (as-contract (stx-transfer? bonus-amount tx-sender tx-sender)))
      
      ;; Clear the claimed bonus pool
      (if (is-eq bonus-type "winner")
        (map-set challenge-sponsor-totals challenge-id
          (merge totals { winner-bonus-pool: u0 })
        )
        (map-set challenge-sponsor-totals challenge-id
          (merge totals { completion-bonus-pool: u0 })
        )
      )
      
      (ok bonus-amount)
    )
  )
)

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok new-fee)
  )
)

;; Update max sponsors per challenge (owner only)
(define-public (set-max-sponsors (new-max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (> new-max u0) err-invalid-amount)
    (var-set max-sponsors-per-challenge new-max)
    (ok new-max)
  )
)

;; Withdraw platform fees (owner only)
(define-public (withdraw-platform-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (ok amount)
  )
)

;; Get sponsorship details
(define-read-only (get-sponsorship (sponsorship-id uint))
  (map-get? challenge-sponsorships sponsorship-id)
)

;; Get challenge sponsorship totals
(define-read-only (get-challenge-sponsorship-totals (challenge-id uint))
  (map-get? challenge-sponsor-totals challenge-id)
)

;; Get sponsor contribution for specific challenge
(define-read-only (get-sponsor-challenge-info (challenge-id uint) (sponsor principal))
  (map-get? challenge-sponsors { challenge-id: challenge-id, sponsor: sponsor })
)

;; Get sponsor global statistics
(define-read-only (get-sponsor-stats (sponsor principal))
  (map-get? sponsor-stats sponsor)
)

;; Get contract settings
(define-read-only (get-contract-settings)
  (ok {
    max-sponsors-per-challenge: (var-get max-sponsors-per-challenge),
    platform-fee-percentage: (var-get platform-fee-percentage),
    next-sponsorship-id: (var-get next-sponsorship-id)
  })
)

;; Calculate potential bonus for a challenge
(define-read-only (calculate-total-bonus (challenge-id uint))
  (match (map-get? challenge-sponsor-totals challenge-id)
    totals (ok {
      winner-bonus: (get winner-bonus-pool totals),
      completion-bonus: (get completion-bonus-pool totals),
      total-bonus: (+ (get winner-bonus-pool totals) (get completion-bonus-pool totals)),
      sponsor-count: (get sponsor-count totals)
    })
    (ok {
      winner-bonus: u0,
      completion-bonus: u0,
      total-bonus: u0,
      sponsor-count: u0
    })
  )
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)
