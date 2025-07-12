(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u101))
(define-constant ERR_CHALLENGE_EXPIRED (err u102))
(define-constant ERR_CHALLENGE_ALREADY_ACCEPTED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_CHALLENGE_NOT_ACTIVE (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_CANNOT_VOTE_OWN_CHALLENGE (err u107))
(define-constant ERR_VOTING_PERIOD_ENDED (err u108))
(define-constant ERR_CHALLENGE_NOT_COMPLETED (err u109))
(define-constant ERR_ALREADY_CLAIMED (err u110))
(define-constant ERR_REPUTATION_TOO_LOW (err u111))
(define-constant ERR_ESCROW_NOT_FOUND (err u112))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u113))
(define-constant ERR_ESCROW_TIMEOUT_NOT_REACHED (err u114))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u115))
(define-constant ERR_CANNOT_RATE_SELF (err u116))
(define-constant ERR_ALREADY_RATED (err u117))

(define-constant INITIAL_REPUTATION u100)
(define-constant MIN_REPUTATION_FOR_CHALLENGES u50)
(define-constant ESCROW_TIMEOUT_BLOCKS u1008)
(define-constant MAX_REPUTATION u1000)

(define-data-var challenge-counter uint u0)
(define-data-var escrow-counter uint u0)

(define-map challenges
  uint
  {
    challenger: principal,
    opponent: (optional principal),
    title: (string-ascii 100),
    description: (string-ascii 500),
    stake-amount: uint,
    deadline: uint,
    voting-deadline: uint,
    status: (string-ascii 20),
    winner: (optional principal),
    challenger-votes: uint,
    opponent-votes: uint,
    total-voters: uint,
    claimed: bool
  }
)

(define-map challenge-votes
  { challenge-id: uint, voter: principal }
  { voted-for: principal }
)

(define-map user-stats
  principal
  {
    challenges-created: uint,
    challenges-won: uint,
    challenges-lost: uint,
    total-staked: uint,
    total-earned: uint
  }
)

(define-map user-reputation
  principal
  {
    score: uint,
    total-ratings: uint,
    positive-ratings: uint,
    challenge-completion-rate: uint,
    last-activity-block: uint,
    reputation-locked: bool
  }
)

(define-map escrow-pools
  uint
  {
    creator: principal,
    beneficiary: principal,
    amount: uint,
    release-condition: (string-ascii 50),
    auto-release-block: uint,
    status: (string-ascii 20),
    created-block: uint,
    related-challenge-id: (optional uint)
  }
)

(define-map reputation-ratings
  { rater: principal, rated: principal, challenge-id: uint }
  { rating: uint, timestamp: uint }
)

(define-public (create-challenge (title (string-ascii 100)) (description (string-ascii 500)) (stake-amount uint) (duration-blocks uint))
  (let
    (
      (challenge-id (+ (var-get challenge-counter) u1))
      (deadline (+ stacks-block-height duration-blocks))
      (voting-deadline (+ deadline u144))
      (user-rep (get-user-reputation tx-sender))
    )
    (asserts! (> stake-amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (get score user-rep) MIN_REPUTATION_FOR_CHALLENGES) ERR_REPUTATION_TOO_LOW)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set challenges challenge-id
      {
        challenger: tx-sender,
        opponent: none,
        title: title,
        description: description,
        stake-amount: stake-amount,
        deadline: deadline,
        voting-deadline: voting-deadline,
        status: "open",
        winner: none,
        challenger-votes: u0,
        opponent-votes: u0,
        total-voters: u0,
        claimed: false
      }
    )
    (var-set challenge-counter challenge-id)
    (update-user-stats tx-sender u1 u0 u0 stake-amount u0)
    (update-reputation-activity tx-sender)
    (ok challenge-id)
  )
)

(define-public (accept-challenge (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (user-rep (get-user-reputation tx-sender))
    )
    (asserts! (is-eq (get status challenge) "open") ERR_CHALLENGE_ALREADY_ACCEPTED)
    (asserts! (< stacks-block-height (get deadline challenge)) ERR_CHALLENGE_EXPIRED)
    (asserts! (not (is-eq tx-sender (get challenger challenge))) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get score user-rep) MIN_REPUTATION_FOR_CHALLENGES) ERR_REPUTATION_TOO_LOW)
    (try! (stx-transfer? (get stake-amount challenge) tx-sender (as-contract tx-sender)))
    (map-set challenges challenge-id
      (merge challenge {
        opponent: (some tx-sender),
        status: "active"
      })
    )
    (update-user-stats tx-sender u1 u0 u0 (get stake-amount challenge) u0)
    (update-reputation-activity tx-sender)
    (ok true)
  )
)

(define-public (submit-completion (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
    )
    (asserts! (is-eq (get status challenge) "active") ERR_CHALLENGE_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get deadline challenge)) ERR_CHALLENGE_EXPIRED)
    (asserts! (< stacks-block-height (get voting-deadline challenge)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (or (is-eq tx-sender (get challenger challenge)) 
                  (is-eq tx-sender (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_NOT_AUTHORIZED)
    (map-set challenges challenge-id
      (merge challenge {
        status: "voting"
      })
    )
    (ok true)
  )
)

(define-public (vote-on-challenge (challenge-id uint) (vote-for principal))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (vote-key { challenge-id: challenge-id, voter: tx-sender })
    )
    (asserts! (is-eq (get status challenge) "voting") ERR_CHALLENGE_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get voting-deadline challenge)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (is-none (map-get? challenge-votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (not (is-eq tx-sender (get challenger challenge))) ERR_CANNOT_VOTE_OWN_CHALLENGE)
    (asserts! (not (is-eq tx-sender (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_CANNOT_VOTE_OWN_CHALLENGE)
    (asserts! (or (is-eq vote-for (get challenger challenge)) 
                  (is-eq vote-for (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_NOT_AUTHORIZED)
    (map-set challenge-votes vote-key { voted-for: vote-for })
    (if (is-eq vote-for (get challenger challenge))
      (map-set challenges challenge-id
        (merge challenge {
          challenger-votes: (+ (get challenger-votes challenge) u1),
          total-voters: (+ (get total-voters challenge) u1)
        })
      )
      (map-set challenges challenge-id
        (merge challenge {
          opponent-votes: (+ (get opponent-votes challenge) u1),
          total-voters: (+ (get total-voters challenge) u1)
        })
      )
    )
    (ok true)
  )
)

(define-public (finalize-challenge (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (challenger (get challenger challenge))
      (opponent (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))
      (challenger-votes (get challenger-votes challenge))
      (opponent-votes (get opponent-votes challenge))
      (total-pot (* (get stake-amount challenge) u2))
    )
    (asserts! (is-eq (get status challenge) "voting") ERR_CHALLENGE_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get voting-deadline challenge)) ERR_VOTING_PERIOD_ENDED)
    (if (> challenger-votes opponent-votes)
      (begin
        (map-set challenges challenge-id
          (merge challenge {
            status: "completed",
            winner: (some challenger)
          })
        )
        (update-user-stats challenger u0 u1 u0 u0 total-pot)
        (update-user-stats opponent u0 u0 u1 u0 u0)
      )
      (if (> opponent-votes challenger-votes)
        (begin
          (map-set challenges challenge-id
            (merge challenge {
              status: "completed",
              winner: (some opponent)
            })
          )
          (update-user-stats opponent u0 u1 u0 u0 total-pot)
          (update-user-stats challenger u0 u0 u1 u0 u0)
        )
        (map-set challenges challenge-id
          (merge challenge {
            status: "tie",
            winner: none
          })
        )
      )
    )
    (ok true)
  )
)

(define-public (claim-winnings (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (winner (get winner challenge))
      (total-pot (* (get stake-amount challenge) u2))
    )
    (asserts! (is-eq (get status challenge) "completed") ERR_CHALLENGE_NOT_COMPLETED)
    (asserts! (not (get claimed challenge)) ERR_ALREADY_CLAIMED)
    (asserts! (is-eq tx-sender (unwrap! winner ERR_NOT_AUTHORIZED)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? total-pot tx-sender (unwrap! winner ERR_NOT_AUTHORIZED))))
    (map-set challenges challenge-id
      (merge challenge { claimed: true })
    )
    (ok total-pot)
  )
)

(define-public (claim-tie-refund (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (stake-amount (get stake-amount challenge))
    )
    (asserts! (is-eq (get status challenge) "tie") ERR_CHALLENGE_NOT_COMPLETED)
    (asserts! (not (get claimed challenge)) ERR_ALREADY_CLAIMED)
    (asserts! (or (is-eq tx-sender (get challenger challenge)) 
                  (is-eq tx-sender (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    (if (is-eq tx-sender (get challenger challenge))
      (map-set challenges challenge-id
        (merge challenge { claimed: true })
      )
      (map-set challenges challenge-id
        (merge challenge { claimed: true })
      )
    )
    (ok stake-amount)
  )
)

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges challenge-id)
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { challenges-created: u0, challenges-won: u0, challenges-lost: u0, total-staked: u0, total-earned: u0 }
    (map-get? user-stats user)
  )
)

(define-read-only (get-challenge-count)
  (var-get challenge-counter)
)

(define-read-only (get-user-vote (challenge-id uint) (voter principal))
  (map-get? challenge-votes { challenge-id: challenge-id, voter: voter })
)

(define-public (create-escrow (beneficiary principal) (amount uint) (condition (string-ascii 50)) (timeout-blocks uint) (challenge-id (optional uint)))
  (let
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (auto-release-block (+ stacks-block-height timeout-blocks))
    )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (not (is-eq tx-sender beneficiary)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrow-pools escrow-id
      {
        creator: tx-sender,
        beneficiary: beneficiary,
        amount: amount,
        release-condition: condition,
        auto-release-block: auto-release-block,
        status: "active",
        created-block: stacks-block-height,
        related-challenge-id: challenge-id
      }
    )
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (release-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-pools escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq (get status escrow) "active") ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (is-eq tx-sender (get creator escrow)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get beneficiary escrow))))
    (map-set escrow-pools escrow-id
      (merge escrow { status: "released" })
    )
    (ok true)
  )
)

(define-public (claim-escrow-timeout (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-pools escrow-id) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq (get status escrow) "active") ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (>= stacks-block-height (get auto-release-block escrow)) ERR_ESCROW_TIMEOUT_NOT_REACHED)
    (asserts! (is-eq tx-sender (get beneficiary escrow)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get beneficiary escrow))))
    (map-set escrow-pools escrow-id
      (merge escrow { status: "timeout-claimed" })
    )
    (ok true)
  )
)

(define-public (rate-user (rated-user principal) (challenge-id uint) (rating uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
      (rating-key { rater: tx-sender, rated: rated-user, challenge-id: challenge-id })
      (current-rep (get-user-reputation rated-user))
    )
    (asserts! (not (is-eq tx-sender rated-user)) ERR_CANNOT_RATE_SELF)
    (asserts! (is-none (map-get? reputation-ratings rating-key)) ERR_ALREADY_RATED)
    (asserts! (is-eq (get status challenge) "completed") ERR_CHALLENGE_NOT_COMPLETED)
    (asserts! (or (is-eq tx-sender (get challenger challenge)) 
                  (is-eq tx-sender (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq rated-user (get challenger challenge)) 
                  (is-eq rated-user (unwrap! (get opponent challenge) ERR_NOT_AUTHORIZED))) ERR_NOT_AUTHORIZED)
    (asserts! (<= rating u5) ERR_NOT_AUTHORIZED)
    (asserts! (>= rating u1) ERR_NOT_AUTHORIZED)
    (map-set reputation-ratings rating-key
      { rating: rating, timestamp: stacks-block-height }
    )
    (let
      (
        (new-total-ratings (+ (get total-ratings current-rep) u1))
        (new-positive-ratings (if (>= rating u4) (+ (get positive-ratings current-rep) u1) (get positive-ratings current-rep)))
        (new-score (calculate-reputation-score new-positive-ratings new-total-ratings))
      )
      (map-set user-reputation rated-user
        (merge current-rep {
          score: new-score,
          total-ratings: new-total-ratings,
          positive-ratings: new-positive-ratings
        })
      )
    )
    (ok true)
  )
)

(define-public (initialize-user-reputation)
  (let
    (
      (current-rep (map-get? user-reputation tx-sender))
    )
    (if (is-none current-rep)
      (begin
        (map-set user-reputation tx-sender
          {
            score: INITIAL_REPUTATION,
            total-ratings: u0,
            positive-ratings: u0,
            challenge-completion-rate: u100,
            last-activity-block: stacks-block-height,
            reputation-locked: false
          }
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-read-only (get-user-reputation (user principal))
  (default-to 
    { 
      score: INITIAL_REPUTATION, 
      total-ratings: u0, 
      positive-ratings: u0, 
      challenge-completion-rate: u100, 
      last-activity-block: stacks-block-height, 
      reputation-locked: false 
    }
    (map-get? user-reputation user)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-pools escrow-id)
)

(define-read-only (get-reputation-rating (rater principal) (rated principal) (challenge-id uint))
  (map-get? reputation-ratings { rater: rater, rated: rated, challenge-id: challenge-id })
)

(define-read-only (calculate-reputation-score (positive uint) (total uint))
  (if (is-eq total u0)
    INITIAL_REPUTATION
    (let
      (
        (percentage (/ (* positive u100) total))
        (base-score (/ (* percentage MAX_REPUTATION) u100))
      )
      (if (> base-score MAX_REPUTATION) MAX_REPUTATION base-score)
    )
  )
)

(define-private (update-reputation-activity (user principal))
  (let
    (
      (current-rep (get-user-reputation user))
    )
    (map-set user-reputation user
      (merge current-rep { last-activity-block: stacks-block-height })
    )
  )
)

(define-private (update-user-stats (user principal) (created uint) (won uint) (lost uint) (staked uint) (earned uint))
  (let
    (
      (current-stats (get-user-stats user))
    )
    (map-set user-stats user
      {
        challenges-created: (+ (get challenges-created current-stats) created),
        challenges-won: (+ (get challenges-won current-stats) won),
        challenges-lost: (+ (get challenges-lost current-stats) lost),
        total-staked: (+ (get total-staked current-stats) staked),
        total-earned: (+ (get total-earned current-stats) earned)
      }
    )
  )
)