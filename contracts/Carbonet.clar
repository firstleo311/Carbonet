(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-registered (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-already-retired (err u106))
(define-constant err-invalid-reason (err u107))
(define-constant err-invalid-certification-level (err u108))
(define-constant err-insufficient-premium (err u109))
(define-constant err-policy-not-found (err u110))
(define-constant err-claim-already-processed (err u111))
(define-constant err-invalid-risk-level (err u112))
(define-constant err-policy-expired (err u113))

(define-data-var total-credits uint u0)
(define-data-var price-per-credit uint u1000000)
(define-data-var total-retired-credits uint u0)
(define-data-var retirement-certificate-counter uint u0)
(define-data-var insurance-pool-balance uint u0)
(define-data-var total-policies-issued uint u0)
(define-data-var base-premium-rate uint u50000)

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

(define-map retirement-records
    { retirement-id: uint }
    {
        credit-holder: principal,
        amount: uint,
        project-id: uint,
        retirement-reason: (string-ascii 100),
        retirement-date: uint,
        certification-level: uint,
        certificate-hash: (buff 32),
        beneficiary: (string-ascii 50),
        additional-info: (string-ascii 200)
    }
)

(define-map retirement-certificates
    { certificate-id: uint }
    {
        retirement-id: uint,
        issuer: principal,
        verification-status: uint,
        environmental-impact: uint,
        compliance-standards: (list 5 (string-ascii 20)),
        audit-trail: (string-ascii 150)
    }
)

(define-map user-retirement-history
    { user: principal, retirement-index: uint }
    { retirement-id: uint }
)

(define-map user-retirement-stats
    { user: principal }
    {
        total-retired: uint,
        retirement-count: uint,
        last-retirement-date: uint,
        highest-certification-level: uint
    }
)

;; Insurance system data maps
(define-map insurance-policies
    { policy-id: uint }
    {
        policy-holder: principal,
        project-id: uint,
        insured-amount: uint,
        premium-paid: uint,
        coverage-start: uint,
        coverage-end: uint,
        risk-level: uint,
        policy-status: uint,
        coverage-percentage: uint
    }
)

(define-map insurance-claims
    { claim-id: uint }
    {
        policy-id: uint,
        claimant: principal,
        claim-amount: uint,
        claim-reason: (string-ascii 150),
        claim-date: uint,
        claim-status: uint,
        assessor: (optional principal),
        payout-amount: uint
    }
)

(define-map project-risk-profiles
    { project-id: uint }
    {
        risk-score: uint,
        historical-failures: uint,
        assessment-date: uint,
        risk-factors: (list 5 (string-ascii 30)),
        mitigation-measures: (string-ascii 200)
    }
)

(define-map user-insurance-stats
    { user: principal }
    {
        total-policies: uint,
        total-premiums-paid: uint,
        total-claims-filed: uint,
        total-payouts-received: uint,
        risk-rating: uint
    }
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

(define-public (retire-credits (amount uint) (project-id uint) (reason (string-ascii 100)) (beneficiary (string-ascii 50)) (additional-info (string-ascii 200)) (certification-level uint))
    (let
        (
            (user-balance (unwrap! (map-get? credit-balances { owner: tx-sender }) err-not-found))
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (retirement-id (var-get total-retired-credits))
            (certificate-hash (sha256 (concat (concat (unwrap-panic (to-consensus-buff? tx-sender)) (unwrap-panic (to-consensus-buff? amount))) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
            (current-stats (default-to { total-retired: u0, retirement-count: u0, last-retirement-date: u0, highest-certification-level: u0 } (map-get? user-retirement-stats { user: tx-sender })))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= (get amount user-balance) amount) err-insufficient-balance)
        (asserts! (>= (len reason) u10) err-invalid-reason)
        (asserts! (<= certification-level u5) err-invalid-certification-level)
        (asserts! (get verification-status project) err-unauthorized)

        (map-set credit-balances
            { owner: tx-sender }
            { amount: (- (get amount user-balance) amount) }
        )

        (map-set retirement-records
            { retirement-id: retirement-id }
            {
                credit-holder: tx-sender,
                amount: amount,
                project-id: project-id,
                retirement-reason: reason,
                retirement-date: stacks-block-height,
                certification-level: certification-level,
                certificate-hash: certificate-hash,
                beneficiary: beneficiary,
                additional-info: additional-info
            }
        )

        (map-set user-retirement-history
            { user: tx-sender, retirement-index: (get retirement-count current-stats) }
            { retirement-id: retirement-id }
        )

        (map-set user-retirement-stats
            { user: tx-sender }
            {
                total-retired: (+ (get total-retired current-stats) amount),
                retirement-count: (+ (get retirement-count current-stats) u1),
                last-retirement-date: stacks-block-height,
                highest-certification-level: (if (> certification-level (get highest-certification-level current-stats)) certification-level (get highest-certification-level current-stats))
            }
        )

        (var-set total-retired-credits (+ retirement-id u1))
        (ok retirement-id)
    )
)

(define-public (issue-retirement-certificate (retirement-id uint) (environmental-impact uint) (compliance-standards (list 5 (string-ascii 20))) (audit-trail (string-ascii 150)))
    (let
        (
            (retirement-record (unwrap! (map-get? retirement-records { retirement-id: retirement-id }) err-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
            (certificate-id (var-get retirement-certificate-counter))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        (asserts! (> environmental-impact u0) err-invalid-amount)

        (map-set retirement-certificates
            { certificate-id: certificate-id }
            {
                retirement-id: retirement-id,
                issuer: tx-sender,
                verification-status: u1,
                environmental-impact: environmental-impact,
                compliance-standards: compliance-standards,
                audit-trail: audit-trail
            }
        )

        (var-set retirement-certificate-counter (+ certificate-id u1))
        (ok certificate-id)
    )
)

(define-public (update-certificate-verification (certificate-id uint) (verification-status uint))
    (let
        (
            (certificate (unwrap! (map-get? retirement-certificates { certificate-id: certificate-id }) err-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        (asserts! (<= verification-status u3) err-invalid-certification-level)

        (map-set retirement-certificates
            { certificate-id: certificate-id }
            (merge certificate { verification-status: verification-status })
        )
        (ok true)
    )
)

(define-public (bulk-retire-credits (retirements (list 10 { amount: uint, project-id: uint, reason: (string-ascii 100), beneficiary: (string-ascii 50), additional-info: (string-ascii 200), certification-level: uint })))
    (let
        (
            (user-balance (unwrap! (map-get? credit-balances { owner: tx-sender }) err-not-found))
            (total-retirement-amount (fold calculate-total-retirement-amount retirements u0))
        )
        (asserts! (>= (get amount user-balance) total-retirement-amount) err-insufficient-balance)
        (asserts! (> total-retirement-amount u0) err-invalid-amount)

        (map-set credit-balances
            { owner: tx-sender }
            { amount: (- (get amount user-balance) total-retirement-amount) }
        )

        (fold process-bulk-retirement retirements u0)
        (ok true)
    )
)

(define-private (calculate-total-retirement-amount (retirement { amount: uint, project-id: uint, reason: (string-ascii 100), beneficiary: (string-ascii 50), additional-info: (string-ascii 200), certification-level: uint }) (acc uint))
    (+ acc (get amount retirement))
)

(define-private (process-bulk-retirement (retirement { amount: uint, project-id: uint, reason: (string-ascii 100), beneficiary: (string-ascii 50), additional-info: (string-ascii 200), certification-level: uint }) (acc uint))
    (let
        (
            (project (default-to { owner: contract-owner, name: "", location: "", total-credits: u0, available-credits: u0, verification-status: false } (map-get? projects { project-id: (get project-id retirement) })))
            (retirement-id (+ (var-get total-retired-credits) acc))
            (certificate-hash (sha256 (concat (concat (unwrap-panic (to-consensus-buff? tx-sender)) (unwrap-panic (to-consensus-buff? (get amount retirement)))) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
            (current-stats (default-to { total-retired: u0, retirement-count: u0, last-retirement-date: u0, highest-certification-level: u0 } (map-get? user-retirement-stats { user: tx-sender })))
        )
        (if (and (>= (len (get reason retirement)) u10) (<= (get certification-level retirement) u5) (get verification-status project))
            (begin
                (map-set retirement-records
                    { retirement-id: retirement-id }
                    {
                        credit-holder: tx-sender,
                        amount: (get amount retirement),
                        project-id: (get project-id retirement),
                        retirement-reason: (get reason retirement),
                        retirement-date: stacks-block-height,
                        certification-level: (get certification-level retirement),
                        certificate-hash: certificate-hash,
                        beneficiary: (get beneficiary retirement),
                        additional-info: (get additional-info retirement)
                    }
                )

                (map-set user-retirement-history
                    { user: tx-sender, retirement-index: (+ (get retirement-count current-stats) acc) }
                    { retirement-id: retirement-id }
                )

                (map-set user-retirement-stats
                    { user: tx-sender }
                    {
                        total-retired: (+ (get total-retired current-stats) (get amount retirement)),
                        retirement-count: (+ (get retirement-count current-stats) u1),
                        last-retirement-date: stacks-block-height,
                        highest-certification-level: (if (> (get certification-level retirement) (get highest-certification-level current-stats)) (get certification-level retirement) (get highest-certification-level current-stats))
                    }
                )

                (var-set total-retired-credits (+ retirement-id u1))
                (+ acc u1)
            )
            acc
        )
    )
)

(define-read-only (get-retirement-record (retirement-id uint))
    (map-get? retirement-records { retirement-id: retirement-id })
)

(define-read-only (get-retirement-certificate (certificate-id uint))
    (map-get? retirement-certificates { certificate-id: certificate-id })
)

(define-read-only (get-user-retirement-stats (user principal))
    (default-to { total-retired: u0, retirement-count: u0, last-retirement-date: u0, highest-certification-level: u0 } (map-get? user-retirement-stats { user: user }))
)

(define-read-only (get-user-retirement-by-index (user principal) (index uint))
    (map-get? user-retirement-history { user: user, retirement-index: index })
)

(define-read-only (get-total-retired-credits)
    (var-get total-retired-credits)
)

(define-read-only (calculate-environmental-impact (user principal))
    (let
        (
            (user-stats (get-user-retirement-stats user))
            (base-impact (get total-retired user-stats))
            (certification-multiplier (+ u1 (/ (get highest-certification-level user-stats) u2)))
        )
        (* base-impact certification-multiplier)
    )
)

(define-read-only (get-retirement-analytics)
    (let
        (
            (total-retired (var-get total-retired-credits))
            (current-block stacks-block-height)
        )
        {
            total-retired-credits: total-retired,
            total-certificates-issued: (var-get retirement-certificate-counter),
            current-block-height: current-block,
            platform-environmental-impact: (* total-retired u100)
        }
    )
)

;; Insurance system functions
(define-public (purchase-insurance-policy (project-id uint) (insured-amount uint) (coverage-duration uint) (coverage-percentage uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (risk-profile (default-to { risk-score: u3, historical-failures: u0, assessment-date: u0, risk-factors: (list), mitigation-measures: "" } (map-get? project-risk-profiles { project-id: project-id })))
            (calculated-premium (calculate-insurance-premium insured-amount (get risk-score risk-profile) coverage-duration coverage-percentage))
            (policy-id (var-get total-policies-issued))
            (user-stats (default-to { total-policies: u0, total-premiums-paid: u0, total-claims-filed: u0, total-payouts-received: u0, risk-rating: u3 } (map-get? user-insurance-stats { user: tx-sender })))
        )
        (asserts! (> insured-amount u0) err-invalid-amount)
        (asserts! (and (>= coverage-percentage u10) (<= coverage-percentage u100)) err-invalid-certification-level)
        (asserts! (> coverage-duration u144) err-invalid-amount) ;; minimum 1 day coverage
        (asserts! (get verification-status project) err-unauthorized)
        
        (try! (stx-transfer? calculated-premium tx-sender (as-contract tx-sender)))
        
        (map-set insurance-policies
            { policy-id: policy-id }
            {
                policy-holder: tx-sender,
                project-id: project-id,
                insured-amount: insured-amount,
                premium-paid: calculated-premium,
                coverage-start: stacks-block-height,
                coverage-end: (+ stacks-block-height coverage-duration),
                risk-level: (get risk-score risk-profile),
                policy-status: u1, ;; active
                coverage-percentage: coverage-percentage
            }
        )
        
        (map-set user-insurance-stats
            { user: tx-sender }
            {
                total-policies: (+ (get total-policies user-stats) u1),
                total-premiums-paid: (+ (get total-premiums-paid user-stats) calculated-premium),
                total-claims-filed: (get total-claims-filed user-stats),
                total-payouts-received: (get total-payouts-received user-stats),
                risk-rating: (get risk-rating user-stats)
            }
        )
        
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) calculated-premium))
        (var-set total-policies-issued (+ policy-id u1))
        (ok policy-id)
    )
)

(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (claim-reason (string-ascii 150)))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-policy-not-found))
            (claim-id (var-get total-policies-issued)) ;; reuse counter for simplicity
            (user-stats (default-to { total-policies: u0, total-premiums-paid: u0, total-claims-filed: u0, total-payouts-received: u0, risk-rating: u3 } (map-get? user-insurance-stats { user: tx-sender })))
        )
        (asserts! (is-eq (get policy-holder policy) tx-sender) err-unauthorized)
        (asserts! (is-eq (get policy-status policy) u1) err-policy-expired) ;; policy must be active
        (asserts! (<= stacks-block-height (get coverage-end policy)) err-policy-expired)
        (asserts! (>= stacks-block-height (get coverage-start policy)) err-policy-expired)
        (asserts! (> claim-amount u0) err-invalid-amount)
        (asserts! (<= claim-amount (get insured-amount policy)) err-invalid-amount)
        (asserts! (>= (len claim-reason) u20) err-invalid-reason)
        
        (map-set insurance-claims
            { claim-id: claim-id }
            {
                policy-id: policy-id,
                claimant: tx-sender,
                claim-amount: claim-amount,
                claim-reason: claim-reason,
                claim-date: stacks-block-height,
                claim-status: u1, ;; pending review
                assessor: none,
                payout-amount: u0
            }
        )
        
        (map-set user-insurance-stats
            { user: tx-sender }
            (merge user-stats { total-claims-filed: (+ (get total-claims-filed user-stats) u1) })
        )
        
        (ok claim-id)
    )
)

(define-public (process-insurance-claim (claim-id uint) (approved bool) (payout-amount uint))
    (let
        (
            (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-not-found))
            (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) err-policy-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
            (user-stats (default-to { total-policies: u0, total-premiums-paid: u0, total-claims-filed: u0, total-payouts-received: u0, risk-rating: u3 } (map-get? user-insurance-stats { user: (get claimant claim) })))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        (asserts! (is-eq (get claim-status claim) u1) err-claim-already-processed) ;; must be pending
        (asserts! (if approved (<= payout-amount (get claim-amount claim)) true) err-invalid-amount)
        (asserts! (if approved (>= (var-get insurance-pool-balance) payout-amount) true) err-insufficient-balance)
        
        (if approved
            (begin
                (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))))
                (var-set insurance-pool-balance (- (var-get insurance-pool-balance) payout-amount))
                (map-set user-insurance-stats
                    { user: (get claimant claim) }
                    (merge user-stats { total-payouts-received: (+ (get total-payouts-received user-stats) payout-amount) })
                )
            )
            true
        )
        
        (map-set insurance-claims
            { claim-id: claim-id }
            (merge claim {
                claim-status: (if approved u2 u3), ;; 2=approved, 3=rejected
                assessor: (some tx-sender),
                payout-amount: payout-amount
            })
        )
        
        (ok true)
    )
)

(define-public (update-project-risk-profile (project-id uint) (risk-score uint) (risk-factors (list 5 (string-ascii 30))) (mitigation-measures (string-ascii 200)))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
            (current-profile (default-to { risk-score: u3, historical-failures: u0, assessment-date: u0, risk-factors: (list), mitigation-measures: "" } (map-get? project-risk-profiles { project-id: project-id })))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        (asserts! (and (<= risk-score u5) (>= risk-score u1)) err-invalid-risk-level)
        
        (map-set project-risk-profiles
            { project-id: project-id }
            {
                risk-score: risk-score,
                historical-failures: (get historical-failures current-profile),
                assessment-date: stacks-block-height,
                risk-factors: risk-factors,
                mitigation-measures: mitigation-measures
            }
        )
        (ok true)
    )
)

(define-public (record-project-failure (project-id uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (verifier-status (unwrap! (map-get? project-verifiers { verifier: tx-sender }) err-unauthorized))
            (current-profile (default-to { risk-score: u3, historical-failures: u0, assessment-date: u0, risk-factors: (list), mitigation-measures: "" } (map-get? project-risk-profiles { project-id: project-id })))
        )
        (asserts! (get active verifier-status) err-unauthorized)
        
        (map-set project-risk-profiles
            { project-id: project-id }
            (merge current-profile {
                historical-failures: (+ (get historical-failures current-profile) u1),
                risk-score: (if (< (get risk-score current-profile) u5) (+ (get risk-score current-profile) u1) u5)
            })
        )
        (ok true)
    )
)

(define-private (calculate-insurance-premium (insured-amount uint) (risk-score uint) (coverage-duration uint) (coverage-percentage uint))
    (let
        (
            (base-rate (var-get base-premium-rate))
            (risk-multiplier (+ u100 (* risk-score u25))) ;; 125%, 150%, 175%, 200%, 225% for risk levels 1-5
            (duration-factor (+ u100 (/ coverage-duration u1440))) ;; additional cost per day
            (coverage-factor (+ u100 coverage-percentage)) ;; percentage-based factor
        )
        (/ (* (* (* insured-amount base-rate) risk-multiplier) duration-factor) (* u100000000 coverage-factor))
    )
)

;; Read-only functions for insurance system
(define-read-only (get-insurance-policy (policy-id uint))
    (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-project-risk-profile (project-id uint))
    (map-get? project-risk-profiles { project-id: project-id })
)

(define-read-only (get-user-insurance-stats (user principal))
    (default-to { total-policies: u0, total-premiums-paid: u0, total-claims-filed: u0, total-payouts-received: u0, risk-rating: u3 } (map-get? user-insurance-stats { user: user }))
)

(define-read-only (calculate-premium-quote (insured-amount uint) (project-id uint) (coverage-duration uint) (coverage-percentage uint))
    (let
        (
            (risk-profile (default-to { risk-score: u3, historical-failures: u0, assessment-date: u0, risk-factors: (list), mitigation-measures: "" } (map-get? project-risk-profiles { project-id: project-id })))
        )
        (calculate-insurance-premium insured-amount (get risk-score risk-profile) coverage-duration coverage-percentage)
    )
)

(define-read-only (get-insurance-pool-status)
    {
        pool-balance: (var-get insurance-pool-balance),
        total-policies: (var-get total-policies-issued),
        base-premium-rate: (var-get base-premium-rate)
    }
)