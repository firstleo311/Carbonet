(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-registered (err u104))
(define-constant err-insufficient-balance (err u105))

(define-data-var total-credits uint u0)
(define-data-var price-per-credit uint u1000000)

(define-map projects 
    { project-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        location: (string-ascii 50),
        total-credits: uint,
        available-credits: uint,
        verification-status: bool
    }
)

(define-map credit-balances
    { owner: principal }
    { amount: uint }
)

(define-map project-verifiers
    { verifier: principal }
    { active: bool }
)

(define-non-fungible-token carbon-credit uint)

(define-public (register-project (name (string-ascii 50)) (location (string-ascii 50)) (initial-credits uint))
    (let
        (
            (project-id (var-get total-credits))
        )
        (asserts! (> initial-credits u0) err-invalid-amount)
        (try! (nft-mint? carbon-credit project-id tx-sender))
        (map-set projects
            { project-id: project-id }
            {
                owner: tx-sender,
                name: name,
                location: location,
                total-credits: initial-credits,
                available-credits: initial-credits,
                verification-status: false
            }
        )
        (var-set total-credits (+ project-id u1))
        (ok project-id)
    )
)

(define-public (verify-project (project-id uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        (map-set projects
            { project-id: project-id }
            (merge project { verification-status: true })
        )
        (ok true)
    )
)

(define-public (add-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set project-verifiers
            { verifier: verifier }
            { active: true }
        )
        (ok true)
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete project-verifiers { verifier: verifier })
        (ok true)
    )
)

(define-public (purchase-credits (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (total-cost (* amount (var-get price-per-credit)))
            (buyer-balance (default-to { amount: u0 } (map-get? credit-balances { owner: tx-sender })))
        )
        (asserts! (get verification-status project) err-unauthorized)
        (asserts! (<= amount (get available-credits project)) err-insufficient-balance)
        (try! (stx-transfer? total-cost tx-sender contract-owner))
        
        (map-set projects
            { project-id: project-id }
            (merge project { available-credits: (- (get available-credits project) amount) })
        )
        
        (map-set credit-balances
            { owner: tx-sender }
            { amount: (+ (get amount buyer-balance) amount) }
        )
        (ok true)
    )
)

(define-public (transfer-credits (recipient principal) (amount uint))
    (let
        (
            (sender-balance (unwrap! (map-get? credit-balances { owner: tx-sender }) err-not-found))
            (recipient-balance (default-to { amount: u0 } (map-get? credit-balances { owner: recipient })))
        )
        (asserts! (>= (get amount sender-balance) amount) err-insufficient-balance)
        
        (map-set credit-balances
            { owner: tx-sender }
            { amount: (- (get amount sender-balance) amount) }
        )
        
        (map-set credit-balances
            { owner: recipient }
            { amount: (+ (get amount recipient-balance) amount) }
        )
        (ok true)
    )
)

(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-credit-balance (owner principal))
    (default-to { amount: u0 } (map-get? credit-balances { owner: owner }))
)

(define-read-only (get-verifier-status (verifier principal))
    (map-get? project-verifiers { verifier: verifier })
)