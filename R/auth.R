verify_login <- function(username, password, config) {
  if (!identical(username, config$username)) return(FALSE)

  if (nzchar(config$password_hash)) {
    return(isTRUE(tryCatch(
      sodium::password_verify(config$password_hash, password),
      error = function(e) FALSE
    )))
  }

  nzchar(config$password) && identical(password, config$password)
}

login_ui <- function(config, message = NULL) {
  div(
    class = "login-page",
    div(
      class = "login-card",
      div(class = "eyebrow", "PRIVATE FOOTBALL INTELLIGENCE"),
      h1("Kazanma Lab"),
      p(class = "login-lead", "İlk 11, skor, golcü, kart ve taktik eşleşmesi için kişisel analiz odan."),
      if (!is.null(message)) div(class = "login-error", message),
      textInput("login_username", "Kullanıcı", placeholder = "Kullanıcı adın"),
      passwordInput("login_password", "Şifre", placeholder = "••••••••••••"),
      actionButton("login_submit", "Analiz odasına gir", class = "btn-login"),
      if (is_demo_environment(config)) {
        div(class = "demo-credential", "Yerel demo: arda / kazanma-lab")
      },
      p(class = "login-footnote", "Üretimde yalnızca HTTPS üzerinden kullan. Şifre GitHub'a yazılmaz.")
    )
  )
}

