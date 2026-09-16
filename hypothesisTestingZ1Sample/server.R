#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

function(input, output) {
    
    # Reactive calculations
    stats <- reactive({
        se <- input$s / sqrt(input$n)
        z_stat <- (input$xbar - input$mu) / se
        
        if (input$hypothesis == "equal") {
            p_val <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
            crit_lower <- qnorm(input$alpha / 2)
            crit_upper <- qnorm(1 - input$alpha / 2)
        } else if (input$hypothesis == "less") {
            p_val <- pnorm(z_stat, lower.tail = TRUE)
            crit_lower <- qnorm(input$alpha)
            crit_upper <- NA
        } else {
            p_val <- pnorm(z_stat, lower.tail = FALSE)
            crit_lower <- NA
            crit_upper <- qnorm(1 - input$alpha)
        }
        
        list(se = se, z = z_stat, p = p_val, crit_l = crit_lower, crit_u = crit_upper)
    })
    
    # 1. Data summary
    output$data_summary <- renderUI({
        withMathJax(
            HTML(paste0("<b>Provided Data:</b><br>",
                        "Sample Mean (\\(\\bar{x}\\)): ", input$xbar, "<br>",
                        "Sample SD (\\(s\\)): ", input$s, "<br>",
                        "Population Mean (\\(\\mu_0\\)): ", input$mu, "<br>",
                        "Sample Size (\\(n\\)): ", input$n))
        )
    })
    
    # 3. Hypotheses
    output$hypotheses <- renderUI({
        if (input$hypothesis == "equal") {
            null_sign <- "\\(=\\)"
            alt_sign <- "\\(\\neq\\)"
        } else if (input$hypothesis == "less") {
            null_sign <- "\\(\\ge\\)"
            alt_sign <- "\\(<\\)"
        } else if (input$hypothesis == "greater") {
            null_sign <- "\\(\\le\\)"
            alt_sign <- "\\(>\\)"
        }
        
        withMathJax(
            HTML(paste0("<b>Hypotheses:</b><br>",
                        "Null Hypothesis (\\(H_0\\)): \\(\\mu \\) ", null_sign, " ", input$mu, "<br>",
                        "Alternative Hypothesis (\\(H_a\\)): \\(\\mu \\) ", alt_sign, " ", input$mu))
        )
    })
    
    # Combined Plot: Null vs Alternative on Z-Scale
    output$main_plot <- renderPlot({
        s <- stats()
        
        # Dynamically scale the x-axis to fit both curves
        x_min <- min(-4, s$z - 4)
        x_max <- max(4, s$z + 4)
        x_vals <- seq(x_min, x_max, length.out = 1000)
        
        df_null <- data.frame(x = x_vals, y = dnorm(x_vals))
        df_alt <- data.frame(x = x_vals, y = dnorm(x_vals, mean = s$z, sd = 1))
        
        p <- ggplot() +
            labs(title = "Hypothesis Testing Distributions (Z-Scale)", x = "Z Score", y = "Density") +
            theme_minimal() +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom")
        
        # Fill the Rejection and Non-Rejection areas for the Null Distribution
        if (input$hypothesis == "equal") {
            p <- p +
                geom_area(data = subset(df_null, x <= s$crit_l), aes(x=x, y=y), fill = "lightgreen", alpha = 0.5) +
                geom_area(data = subset(df_null, x >= s$crit_u), aes(x=x, y=y), fill = "lightgreen", alpha = 0.5) +
                geom_area(data = subset(df_null, x > s$crit_l & x < s$crit_u), aes(x=x, y=y), fill = "lightcoral", alpha = 0.5) +
                geom_vline(xintercept = c(s$crit_l, s$crit_u), linetype = "dashed", color = "firebrick", linewidth = 0.9)
        } else if (input$hypothesis == "less") {
            p <- p +
                geom_area(data = subset(df_null, x <= s$crit_l), aes(x=x, y=y), fill = "lightgreen", alpha = 0.5) +
                geom_area(data = subset(df_null, x > s$crit_l), aes(x=x, y=y), fill = "lightcoral", alpha = 0.5) +
                geom_vline(xintercept = s$crit_l, linetype = "dashed", color = "firebrick", linewidth = 0.9)
        } else {
            p <- p +
                geom_area(data = subset(df_null, x >= s$crit_u), aes(x=x, y=y), fill = "lightgreen", alpha = 0.5) +
                geom_area(data = subset(df_null, x < s$crit_u), aes(x=x, y=y), fill = "lightcoral", alpha = 0.5) +
                geom_vline(xintercept = s$crit_u, linetype = "dashed", color = "firebrick", linewidth = 0.9)
        }
        
        # Draw both density curves
        p <- p + 
            geom_line(data = df_null, aes(x = x, y = y, color = "Null Hypothesis"), linewidth = 1) +
            geom_line(data = df_alt, aes(x = x, y = y, color = "Alternative Hypothesis"), linewidth = 1, linetype = "dashed") +
            scale_color_manual(name = "Distribution", values = c("Null Hypothesis" = "black", "Alternative Hypothesis" = "darkorange"))
        
        # Add Computed Test Statistic Line
        p <- p + geom_vline(xintercept = s$z, color = "blue", linewidth = 1.2) +
            annotate("text", x = s$z, y = max(df_null$y) * 0.9, label = "Computed Z", color = "blue", angle = 90, vjust = -0.5)
        
        p
    })
    
    # 2 & 5. Assumptions and Distribution of test statistics
    output$assumptions <- renderUI({
        withMathJax(
            HTML(paste0("<b>Assumptions & Test Statistic Distribution:</b><br>",
                        "1. The data is normally distributed.<br>",
                        "2. The observations are independent.<br>",
                        "If the null hypothesis is true and assumptions are met, the test statistic follows a standard normal distribution."))
        )
    })
    
    # 6, 7, 8, 9, 10. Results and Conclusion
    output$results <- renderUI({
        s <- stats()
        decision <- if(s$p < input$alpha) {
            "We <b>reject</b> the null hypothesis."
        } else {
            "We <b>fail to reject</b> the null hypothesis."
        }
        
        withMathJax(
            HTML(paste0("<b>Statistical Decision & Results:</b><br>",
                        "Test Statistic (\\(Z\\)): \\(Z = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ", round(s$z, 3), "<br>",
                        "p-value: ", round(s$p, 4), "<br>",
                        "Decision: ", decision, "<br>",
                        "<b>Conclusion:</b> With a p-value of ", round(s$p, 4), " at an alpha level of ", input$alpha, ", ", tolower(decision)))
        )
    })
}
