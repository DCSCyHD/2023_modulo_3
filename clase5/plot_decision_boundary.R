plot_decision_boundary <- function(
      data, model, x_names, y_name, n_points = 100, model_name=""
    ) {
  
    # Crossing genera todas las combinaciones posibles entre 2 vectores:
    # Observaciones simuladas a partir de valores posibles de 2 variables (x)
    data_simulada <- tidyr::crossing(
      X1 = seq(min(data[[x_names[1]]]),
               max(data[[x_names[1]]]),
               length = n_points),
      X2 = seq(min(data[[x_names[2]]]),
               max(data[[x_names[2]]]),
               length = n_points)
    )
    names(data_simulada) <- x_names
    print(paste0("Cantidad de datos simulados: ", nrow(data_simulada)))
    
    # Se utiliza el modelo para asignar el valor predicho (clase predicha)
    data_simulada$y_pred <- predict(model, data_simulada)$.pred_class

    data$y_pred = predict(model, data)$.pred_class

    # Visualmente
    ggplot(data = data_simulada,
            aes(x = .data[[x_names[1]]], 
                y = .data[[x_names[2]]], 
                fill = .data$y_pred
            )
      ) +
      geom_raster(alpha=0.2, show.legend = FALSE) +
      geom_point(
        data = data,
        mapping = aes(
          x = .data[[x_names[1]]],
          y = .data[[x_names[2]]],
          shape = factor(.data[[y_name]]),
          color = factor(.data[["y_pred"]]),
        ), fill="grey",
        size = 2, alpha = .5) +
      scale_color_manual(values=c("red","blue"))+
      scale_fill_manual(values=c("red","blue"))+
      scale_shape_manual(values=c(4,1))+
      labs(x = x_names[1],
           y = x_names[2],
           color = "Valor predicho",
           shape = "Valor observado",
           title = model_name)+
      theme_bw()+
      theme(legend.position='bottom',
            legend.box = "vertical")
}



# # Comparación de modelos --------------------------------------------------
# p_knn5 <- plot_decision_boundary(data=base_juguete,
#                        model=knn_fiteado5,
#                        x_names=c("horas_trabajo_mercado","ingreso_familiar"),
#                        y_name="realiza_trabajo_domestico",
#                        n_points=100,
#                        model_name = "KNN (k=5)"
#                        )
# 
# p_lda <- plot_decision_boundary(data=base_juguete,
#                                 model=lda_fiteado,
#                                 x_names=c("horas_trabajo_mercado","ingreso_familiar"),
#                                 y_name="realiza_trabajo_domestico",
#                                 n_points=100,
#                                 model_name = "LDA"
# )
# 
# 
# p_reglog <- plot_decision_boundary(data=base_juguete,
#                                 model=logistica_fiteada,
#                                 x_names=c("horas_trabajo_mercado","ingreso_familiar"),
#                                 y_name="realiza_trabajo_domestico",
#                                 n_points=100,
#                                 model_name = "Regresión logística"
# )
# 
# 
# p_knn5 | p_lda | p_reglog
# 
# # KNN N vecinos -----------------------------------------------------------
# knn_fiteado10 <- nearest_neighbor(neighbors = 10,
#                                  weight_func = "rectangular") %>%
#   set_engine("kknn") %>%
#   set_mode("classification") %>%
#   fit(formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado +
#         ingreso_familiar,
#       data = base_juguete)
# 
# p_knn10 <- plot_decision_boundary(data=base_juguete,
#                                  model=knn_fiteado10,
#                                  x_names=c("horas_trabajo_mercado","ingreso_familiar"),
#                                  y_name="realiza_trabajo_domestico",
#                                  n_points=100,
#                                  model_name = "KNN (k = 10)"
# )
# 
# (p_knn5 | p_knn10) /  (p_lda | p_reglog)
# 