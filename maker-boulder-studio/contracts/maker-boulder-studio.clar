;; Maker Boulder Studio - Digital Collectibles Platform
;; A comprehensive smart contract for creator economy and IP protection

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-percentage (err u104))
(define-constant err-insufficient-balance (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (basis points)
(define-data-var reputation-reward uint u100) ;; tokens per verification

;; Data Maps

;; Creative DNA Registry - stores asset metadata and provenance
(define-map creative-assets
  { asset-id: uint }
  {
    creator: principal,
    ipfs-hash: (string-ascii 64),
    creation-timestamp: uint,
    device-signature: (string-ascii 128),
    verification-layers: uint,
    is-active: bool
  }
)

;; Royalty Configuration - defines revenue distribution
(define-map royalty-splits
  { asset-id: uint }
  {
    primary-creator-percentage: uint,
    collaborators: (list 10 { collaborator: principal, percentage: uint }),
    derivative-percentage: uint
  }
)

;; Reputation Scores - tracks community member contributions
(define-map reputation-scores
  { user: principal }
  {
    verification-count: uint,
    curation-score: uint,
    total-tokens-earned: uint,
    governance-power: uint
  }
)

;; Asset Ownership & Licensing
(define-map asset-ownership
  { asset-id: uint }
  { owner: principal }
)

(define-map licensing-agreements
  { asset-id: uint, licensee: principal }
  {
    license-type: (string-ascii 32),
    expiry-block: uint,
    fee-paid: uint,
    is-active: bool
  }
)

;; Revenue tracking
(define-map asset-revenue
  { asset-id: uint }
  { total-earned: uint }
)

;; Counters
(define-data-var asset-id-nonce uint u0)

;; Read-only functions

(define-read-only (get-asset-details (asset-id uint))
  (map-get? creative-assets { asset-id: asset-id })
)

(define-read-only (get-royalty-info (asset-id uint))
  (map-get? royalty-splits { asset-id: asset-id })
)

(define-read-only (get-reputation (user principal))
  (default-to 
    { verification-count: u0, curation-score: u0, total-tokens-earned: u0, governance-power: u0 }
    (map-get? reputation-scores { user: user })
  )
)

(define-read-only (get-asset-owner (asset-id uint))
  (map-get? asset-ownership { asset-id: asset-id })
)

(define-read-only (get-license (asset-id uint) (licensee principal))
  (map-get? licensing-agreements { asset-id: asset-id, licensee: licensee })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Public functions

;; Register new creative asset with DNA fingerprint
(define-public (register-asset 
  (ipfs-hash (string-ascii 64))
  (device-signature (string-ascii 128))
  (verification-layers uint))
  
  (let
    (
      (new-asset-id (+ (var-get asset-id-nonce) u1))
    )
    
    ;; Store asset metadata
    (map-set creative-assets
      { asset-id: new-asset-id }
      {
        creator: tx-sender,
        ipfs-hash: ipfs-hash,
        creation-timestamp: block-height,
        device-signature: device-signature,
        verification-layers: verification-layers,
        is-active: true
      }
    )
    
    ;; Set initial ownership
    (map-set asset-ownership
      { asset-id: new-asset-id }
      { owner: tx-sender }
    )
    
    ;; Initialize royalty split (100% to creator by default)
    (map-set royalty-splits
      { asset-id: new-asset-id }
      {
        primary-creator-percentage: u10000, ;; 100% in basis points
        collaborators: (list),
        derivative-percentage: u0
      }
    )
    
    ;; Initialize revenue tracking
    (map-set asset-revenue
      { asset-id: new-asset-id }
      { total-earned: u0 }
    )
    
    ;; Increment nonce
    (var-set asset-id-nonce new-asset-id)
    
    (ok new-asset-id)
  )
)

;; Configure royalty splits for an asset
(define-public (set-royalty-splits
  (asset-id uint)
  (primary-percentage uint)
  (collaborators (list 10 { collaborator: principal, percentage: uint }))
  (derivative-percentage uint))
  
  (let
    (
      (asset (unwrap! (map-get? creative-assets { asset-id: asset-id }) err-not-found))
      (total-percentage (+ primary-percentage derivative-percentage (fold + (map get-percentage collaborators) u0)))
    )
    
    ;; Verify caller is creator
    (asserts! (is-eq tx-sender (get creator asset)) err-unauthorized)
    
    ;; Verify percentages sum to 100% (10000 basis points)
    (asserts! (is-eq total-percentage u10000) err-invalid-percentage)
    
    ;; Update royalty configuration
    (map-set royalty-splits
      { asset-id: asset-id }
      {
        primary-creator-percentage: primary-percentage,
        collaborators: collaborators,
        derivative-percentage: derivative-percentage
      }
    )
    
    (ok true)
  )
)

;; Helper function for percentage calculation
(define-private (get-percentage (collab { collaborator: principal, percentage: uint }))
  (get percentage collab)
)

;; Distribute revenue with smart royalty cascades
(define-public (distribute-revenue (asset-id uint) (amount uint))
  (let
    (
      (asset (unwrap! (map-get? creative-assets { asset-id: asset-id }) err-not-found))
      (royalty-info (unwrap! (map-get? royalty-splits { asset-id: asset-id }) err-not-found))
      (platform-fee (/ (* amount (var-get platform-fee-percentage)) u10000))
      (distributable (- amount platform-fee))
    )
    
    ;; Calculate and distribute to primary creator
    (let
      (
        (creator-amount (/ (* distributable (get primary-creator-percentage royalty-info)) u10000))
      )
      (try! (stx-transfer? creator-amount tx-sender (get creator asset)))
      
      ;; Distribute to collaborators
      (distribute-to-collaborators asset-id distributable (get collaborators royalty-info))
      
      ;; Update revenue tracking
      (map-set asset-revenue
        { asset-id: asset-id }
        { total-earned: (+ amount (default-to u0 (get total-earned (map-get? asset-revenue { asset-id: asset-id })))) }
      )
      
      (ok true)
    )
  )
)

;; Helper function to distribute to collaborators
(define-private (distribute-to-collaborators 
  (asset-id uint)
  (distributable uint)
  (collaborators (list 10 { collaborator: principal, percentage: uint })))
  
  (begin
    (fold distribute-single-collaborator 
      collaborators 
      distributable)
    true
  )
)

(define-private (distribute-single-collaborator
  (collab { collaborator: principal, percentage: uint })
  (distributable uint))
  
  (let
    (
      (collab-amount (/ (* distributable (get percentage collab)) u10000))
    )
    (match (stx-transfer? collab-amount tx-sender (get collaborator collab))
      success distributable
      error distributable
    )
  )
)

;; Verify authenticity and earn reputation
(define-public (verify-asset (asset-id uint))
  (let
    (
      (asset (unwrap! (map-get? creative-assets { asset-id: asset-id }) err-not-found))
      (current-rep (get-reputation tx-sender))
      (reward (var-get reputation-reward))
    )
    
    ;; Update reputation score
    (map-set reputation-scores
      { user: tx-sender }
      {
        verification-count: (+ (get verification-count current-rep) u1),
        curation-score: (+ (get curation-score current-rep) u10),
        total-tokens-earned: (+ (get total-tokens-earned current-rep) reward),
        governance-power: (+ (get governance-power current-rep) u1)
      }
    )
    
    (ok true)
  )
)

;; Create licensing agreement
(define-public (create-license
  (asset-id uint)
  (license-type (string-ascii 32))
  (duration-blocks uint)
  (fee uint))
  
  (let
    (
      (asset (unwrap! (map-get? creative-assets { asset-id: asset-id }) err-not-found))
    )
    
    ;; Transfer license fee
    (try! (stx-transfer? fee tx-sender (get creator asset)))
    
    ;; Create license agreement
    (map-set licensing-agreements
      { asset-id: asset-id, licensee: tx-sender }
      {
        license-type: license-type,
        expiry-block: (+ block-height duration-blocks),
        fee-paid: fee,
        is-active: true
      }
    )
    
    ;; Distribute revenue
    (try! (distribute-revenue asset-id fee))
    
    (ok true)
  )
)

;; Transfer asset ownership
(define-public (transfer-asset (asset-id uint) (new-owner principal))
  (let
    (
      (current-owner (unwrap! (map-get? asset-ownership { asset-id: asset-id }) err-not-found))
    )
    
    ;; Verify caller is current owner
    (asserts! (is-eq tx-sender (get owner current-owner)) err-unauthorized)
    
    ;; Transfer ownership
    (map-set asset-ownership
      { asset-id: asset-id }
      { owner: new-owner }
    )
    
    (ok true)
  )
)

;; Admin functions

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-percentage) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

(define-public (set-reputation-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set reputation-reward new-reward)
    (ok true)
  )
)