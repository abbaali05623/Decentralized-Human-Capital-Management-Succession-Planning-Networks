;; Development Coordination System
;; Coordinates training and development programs for succession candidates

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-NOT-FOUND (err u301))
(define-constant ERR-INVALID-STATUS (err u302))
(define-constant ERR-PROGRAM-FULL (err u303))

(define-map development-programs
  { program-id: uint }
  {
    name: (string-ascii 100),
    coordinator: principal,
    created-at: uint,
    duration-weeks: uint,
    max-participants: uint,
    current-participants: uint,
    status: (string-ascii 20),
    target-skills: (list 10 (string-ascii 50))
  }
)

(define-map program-enrollments
  { program-id: uint, participant: principal }
  {
    enrolled-at: uint,
    progress-percentage: uint,
    completion-status: (string-ascii 20),
    final-score: uint
  }
)

(define-map development-milestones
  { program-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    target-date: uint,
    completion-criteria: (string-ascii 300),
    weight-percentage: uint
  }
)

(define-data-var next-program-id uint u1)

(define-public (create-development-program
  (name (string-ascii 100))
  (duration-weeks uint)
  (max-participants uint)
  (target-skills (list 10 (string-ascii 50))))
  (let ((program-id (var-get next-program-id)))
    (map-set development-programs
      { program-id: program-id }
      {
        name: name,
        coordinator: tx-sender,
        created-at: block-height,
        duration-weeks: duration-weeks,
        max-participants: max-participants,
        current-participants: u0,
        status: "active",
        target-skills: target-skills
      }
    )

    (var-set next-program-id (+ program-id u1))
    (ok program-id)
  )
)

(define-public (enroll-participant (program-id uint) (participant principal))
  (let ((program (unwrap! (map-get? development-programs { program-id: program-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator program)) ERR-NOT-AUTHORIZED)
    (asserts! (< (get current-participants program) (get max-participants program)) ERR-PROGRAM-FULL)
    (asserts! (is-eq (get status program) "active") ERR-INVALID-STATUS)

    (map-set program-enrollments
      { program-id: program-id, participant: participant }
      {
        enrolled-at: block-height,
        progress-percentage: u0,
        completion-status: "enrolled",
        final-score: u0
      }
    )

    (map-set development-programs
      { program-id: program-id }
      (merge program { current-participants: (+ (get current-participants program) u1) })
    )

    (ok true)
  )
)

(define-public (update-progress
  (program-id uint)
  (participant principal)
  (progress-percentage uint))
  (let ((program (unwrap! (map-get? development-programs { program-id: program-id }) ERR-NOT-FOUND))
        (enrollment (unwrap! (map-get? program-enrollments { program-id: program-id, participant: participant }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator program)) ERR-NOT-AUTHORIZED)
    (asserts! (<= progress-percentage u100) (err u304))

    (map-set program-enrollments
      { program-id: program-id, participant: participant }
      (merge enrollment {
        progress-percentage: progress-percentage,
        completion-status: (if (>= progress-percentage u100) "completed" "in-progress")
      })
    )

    (ok true)
  )
)

(define-public (add-milestone
  (program-id uint)
  (milestone-id uint)
  (description (string-ascii 200))
  (target-date uint)
  (completion-criteria (string-ascii 300))
  (weight-percentage uint))
  (let ((program (unwrap! (map-get? development-programs { program-id: program-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator program)) ERR-NOT-AUTHORIZED)

    (map-set development-milestones
      { program-id: program-id, milestone-id: milestone-id }
      {
        description: description,
        target-date: target-date,
        completion-criteria: completion-criteria,
        weight-percentage: weight-percentage
      }
    )

    (ok true)
  )
)

(define-read-only (get-program-info (program-id uint))
  (map-get? development-programs { program-id: program-id })
)

(define-read-only (get-enrollment-info (program-id uint) (participant principal))
  (map-get? program-enrollments { program-id: program-id, participant: participant })
)

(define-read-only (get-milestone-info (program-id uint) (milestone-id uint))
  (map-get? development-milestones { program-id: program-id, milestone-id: milestone-id })
)
