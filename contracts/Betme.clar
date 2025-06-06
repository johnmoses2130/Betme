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

(define-data-var challenge-counter uint u0)

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

(define-public (create-challenge (title (string-ascii 100)) (description (string-ascii 500)) (stake-amount uint) (duration-blocks uint))
  (let
    (
      (challenge-id (+ (var-get challenge-counter) u1))
      (deadline (+ stacks-block-height duration-blocks))
      (voting-deadline (+ deadline u144))
    )
    (asserts! (> stake-amount u0) ERR_INSUFFICIENT_FUNDS)
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
    (ok challenge-id)
  )
)

(define-public (accept-challenge (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
    )
    (asserts! (is-eq (get status challenge) "open") ERR_CHALLENGE_ALREADY_ACCEPTED)
    (asserts! (< stacks-block-height (get deadline challenge)) ERR_CHALLENGE_EXPIRED)
    (asserts! (not (is-eq tx-sender (get challenger challenge))) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? (get stake-amount challenge) tx-sender (as-contract tx-sender)))
    (map-set challenges challenge-id
      (merge challenge {
        opponent: (some tx-sender),
        status: "active"
      })
    )
    (update-user-stats tx-sender u1 u0 u0 (get stake-amount challenge) u0)
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