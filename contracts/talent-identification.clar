;; Talent Identification System
;; Manages identification and tracking of succession candidates

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-EXISTS (err u202))
(define-constant ERR-INVALID-SCORE (err u203))

(define-map talent-pool
  { talent-id: uint }
  {
    candidate: principal,
    identified-by: principal,
    identified-at: uint,
    position-target: (string-ascii 100),
    current-role: (string-ascii 100),
    potential-score: uint,
    readiness-level: uint,
    active: bool
  }
)

(define-map candidate-skills
  { talent-id: uint, skill: (string-ascii 50) }
  {
    proficiency-level: uint,
    assessed-at: uint,
    assessor: principal
  }
)

(define-data-var next-talent-id uint u1)

(define-public (identify-talent
  (candidate principal)
  (position-target (string-ascii 100))
  (current-role (string-ascii 100))
  (potential-score uint))
  (let ((talent-id (var-get next-talent-id)))
    (asserts! (<= potential-score u100) ERR-INVALID-SCORE)

    (map-set talent-pool
      { talent-id: talent-id }
      {
        candidate: candidate,
        identified-by: tx-sender,
        identified-at: block-height,
        position-target: position-target,
        current-role: current-role,
        potential-score: potential-score,
        readiness-level: u0,
        active: true
      }
    )

    (var-set next-talent-id (+ talent-id u1))
    (ok talent-id)
  )
)

(define-public (update-potential-score (talent-id uint) (new-score uint))
  (let ((talent (unwrap! (map-get? talent-pool { talent-id: talent-id }) ERR-NOT-FOUND)))
    (asserts! (<= new-score u100) ERR-INVALID-SCORE)
    (asserts! (is-eq tx-sender (get identified-by talent)) ERR-NOT-AUTHORIZED)

    (map-set talent-pool
      { talent-id: talent-id }
      (merge talent { potential-score: new-score })
    )

    (ok true)
  )
)

(define-public (assess-skill
  (talent-id uint)
  (skill (string-ascii 50))
  (proficiency-level uint))
  (let ((talent (unwrap! (map-get? talent-pool { talent-id: talent-id }) ERR-NOT-FOUND)))
    (asserts! (<= proficiency-level u10) ERR-INVALID-SCORE)

    (map-set candidate-skills
      { talent-id: talent-id, skill: skill }
      {
        proficiency-level: proficiency-level,
        assessed-at: block-height,
        assessor: tx-sender
      }
    )

    (ok true)
  )
)

(define-public (deactivate-talent (talent-id uint))
  (let ((talent (unwrap! (map-get? talent-pool { talent-id: talent-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get identified-by talent)) ERR-NOT-AUTHORIZED)

    (map-set talent-pool
      { talent-id: talent-id }
      (merge talent { active: false })
    )

    (ok true)
  )
)

(define-read-only (get-talent-info (talent-id uint))
  (map-get? talent-pool { talent-id: talent-id })
)

(define-read-only (get-skill-assessment (talent-id uint) (skill (string-ascii 50)))
  (map-get? candidate-skills { talent-id: talent-id, skill: skill })
)

(define-read-only (is-active-talent (talent-id uint))
  (match (map-get? talent-pool { talent-id: talent-id })
    talent (get active talent)
    false
  )
)
