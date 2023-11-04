# Função para calcular o intervalo de confiança para a média
calcular_intervalo_confianca_media <- function(media_amostra, desvio_padrao, n, nivel_confianca) {
  z <- qnorm(1 - nivel_confianca / 2)  # Valor z correspondente ao nível de confiança
  erro_padrao <- desvio_padrao / sqrt(n)
  limite_inferior <- media_amostra - z * erro_padrao
  limite_superior <- media_amostra + z * erro_padrao
  return(c(limite_inferior, limite_superior))
}

# Supondo as estatísticas fornecidas como 5% = 1.64  
media_idade <- 922.1429
desvio_padrao_idade <- 491.8645
n_idade <- 35
nivel_confianca <- 0.5
media_circunferencia <- 115.8571
n_circunferencia <- 35
desvio_padrao_circunferencia <- 57.4882

# Calcular intervalo de confiança para a média da Idade
intervalo_idade <- calcular_intervalo_confianca_media(media_idade, desvio_padrao_idade, n_idade, nivel_confianca)

# Calcular intervalo de confiança para a média da Circunferência
intervalo_circunferencia <- calcular_intervalo_confianca_media(media_circunferencia, desvio_padrao_circunferencia, n_circunferencia, nivel_confianca)

# Exibir os resultados
cat("Intervalo de confiança para a média da Idade (95%): [", intervalo_idade[1], ", ", intervalo_idade[2], "]\n")
cat("Intervalo de confiança para a média da Circunferência (95%): [", intervalo_circunferencia[1], ", ", intervalo_circunferencia[2], "]\n")

intervalo_confianca_variancia <- function(n, variancia_amostra, nivel_confianca) {
  alpha <- 1 - nivel_confianca
  graus_liberdade <- n - 1
  chi_quad_alpha_meio <- qchisq(alpha / 2, df = graus_liberdade)
  chi_quad_1_menos_alpha_meio <- qchisq(1 - alpha / 2, df = graus_liberdade)

  limite_inferior <- (n - 1) * variancia_amostra / chi_quad_1_menos_alpha_meio
  limite_superior <- (n - 1) * variancia_amostra / chi_quad_alpha_meio

  return(list(limite_inferior = limite_inferior, limite_superior = limite_superior))
}

n <- 20 # Tamanho da amostra
variancia_amostra <- 241930.7 # Variância da amostra
nivel_confianca <- 0.5 # Nível de confiança de 5%

resultado_intervalo <- intervalo_confianca_variancia(n, variancia_amostra, nivel_confianca)
cat("Limite Inferior variancia:", resultado_intervalo$limite_inferior, "\n")
cat("Limite Superior variancia:", resultado_intervalo$limite_superior, "\n")
