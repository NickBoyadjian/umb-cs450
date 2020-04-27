 [(d:apply? exp) 
    ;; lambda-eff
    (define lambda-eff (d:eval-exp mem env (d:apply-func exp)))
    (define lambda-eff-env (d:closure-env (eff-result lambda-eff)))
    (define lambda-eff-state (eff-state lambda-eff))
    (define lamda-eff-lambda (d:closure-decl (eff-result lambda-eff)))

    ;; ea+m2
    (define va (d:eval-exp lambda-eff-state env (d:apply-arg1 exp)))
    
    ;; eb-m3
    (define Eb* 
        (environ-push 
            (eff-state va) 
            lambda-eff-env 
            (d:lambda-param1 (d:closure-decl (eff-result lambda-eff))) 
            (eff-result va))) 
         
         (d:eval-term 
            (eff-state Eb*) 
            (eff-result Eb*) 
            (d:lambda-body lamda-eff-lambda))]