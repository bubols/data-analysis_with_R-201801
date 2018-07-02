# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)

# Ricardo Bubols Pinheiro e Rodrigo Martinelli Toledo

library( tidyverse )
library( dplyr )
library( ggplot2 )
library(lubridate)

departments <- read_csv('project/departments.csv')                   # Cadastro de Departamentos
aisles <- read_csv('project/aisles.csv')                             # Cadastro de 'Corredores'
products <- read_csv('project/products.csv')                         # Cadastro de Produtos

insta_orders <- read_csv( 'project/orders_instacart.csv' )           # Amostra de pedidos de usuários
insta_products <- read_csv( 'project/order_products_instacart.csv' ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?

anti_join(products, insta_products, by = 'product_id') %>%
    count()

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

combined_data <- products %>%
    inner_join(aisles, by = 'aisle_id') %>%
    inner_join(departments, by = 'department_id')
  

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

top10combinations <- combined_data %>%
    group_by(department_id, aisle_id, department, aisle) %>%
    summarise(quantidade = n()) %>%
    arrange(desc(quantidade)) %>%
    head(10) %>%
    ungroup()

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

percentage_pairs <- products %>%
  inner_join(top10combinations, by = 'aisle_id', 'department_id') %>%
  inner_join(insta_products, by = 'product_id') %>%
  distinct(order_id) %>%
  summarise((n() / count(insta_orders) * 100)) %>% View()
  
#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

categorized <- products %>%
  inner_join(insta_products, by = 'product_id') %>%
  inner_join(top10combinations, by = 'aisle_id', 'department_id') %>% 
  filter(aisle != 'missing' | department != 'missing') %>% View()

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   # Transforme as variáveis user_id, department e aisle em factor
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)

   # Este dataframe deverá ser utilizado em todas as atividades seguintes

df <- products %>%
  inner_join(insta_products, by = 'product_id') %>%
  inner_join(insta_orders, by = 'order_id') %>%
  inner_join(aisles, by = 'aisle_id') %>%
  inner_join(departments, by = 'department_id') %>%
  inner_join(top10combinations, by = 'aisle_id', 'department_id') %>%
  select(order_id, order_number, order_hour_of_day, order_dow, user_id, days_since_prior_order, product_id, product_name, aisle_id, aisle.x,
department_id.x, department.x) %>%
  mutate(user_id = factor(user_id),
         aisle = factor(aisle.x),
         department = factor(department.x),
         order_hour_of_day = factor(order_hour_of_day, ordered = TRUE)) 

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos

five_times <- df %>%
  group_by(order_hour_of_day) %>%
  summarise(count_users = n_distinct(user_id)) %>%
  arrange(desc(count_users)) %>%
  head(n = 5) %>%
  ungroup()

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

top15 <- df %>%
  inner_join(five_times, by = 'order_hour_of_day') %>%
  group_by(product_id, product_name) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade)) %>%
  head(15) %>%
  ungroup()
  

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 

mean_sales <- top15 %>%
  inner_join(products, by = 'product_id') %>%
  inner_join(df, by = 'product_id') %>%
  group_by(product_id, product_name, order_hour_of_day, order_dow) %>%
  summarise(quantidade = n()) %>%
  group_by(product_id, product_name, order_hour_of_day) %>%
  summarise(media_quantidade = mean(quantidade)) %>%
  ungroup()

  ggplot(data = mean_sales, aes(x = order_hour_of_day, y = media_quantidade, group = product_name)) + 
    geom_line(aes(colour = product_name)) +
    geom_point(aes(colour = product_name)) + 
    labs(x = 'Hour of the Day', y = 'Amount', title = 'Average Hourly Sales', colour = 'Product Name')

  # Apesar dos produtos terem um padrão similar ao longo do dia, o 'Organic Whole String Cheese' representa o maior volume de vendas.
  # Além disso, este produto tem um pico de vendas no horário 16, enquanto os demais vendem menos nesse horário.

#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 

statistics <- df %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(quantidade = n_distinct(order_id)) %>%
  summarise(mean = mean(quantidade), sd = sd(quantidade), median = median(quantidade), min = min(quantidade), max = max(quantidade)) %>%
  ungroup()

    # Sim, é gaussiana. Os valores encontrados são simétricos.

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

graph_avg_product_hour <- df %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(quantidade = n()) %>%
  ungroup() %>%
  group_by(order_hour_of_day) %>%
  mutate(hour = as.numeric(order_hour_of_day),
         sd_min = mean(quantidade) - 1 * sd(quantidade),
         sd_max = mean(quantidade) + 1 * sd(quantidade)) %>%
  ungroup() 
  
ggplot(data = graph_avg_product_hour, aes( x = hour, y = quantidade, ymin = sd_min, ymax = sd_max )) +
  geom_ribbon(fill = "gray", alpha = 0.5) +
  geom_jitter(alpha = 0.2, height = 0, width = 0.3) +
  labs(x = 'Hour of the Day', y = 'Average amount of products', title = 'Graph 11')

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

order_per_hour <- df %>%
  mutate = (quantidade = 1) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(total = sum(quantidade)) %>%
  ungroup()

ggplot(data = order_per_hour, aes(x = order_dow, y = quantidade, group = order_dow)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from = 0, to = 6, by = 1)) +
  labs(x = 'Day of the week', y = 'Orders', title = 'Graph 12')
  
#13 # Identifique, por usuário, o tempo médio entre pedidos

avg_time <- df %>%
  group_by(user_id) %>%
  summarise(avg_days = mean(days_since_prior_order)) %>%
  ungroup()

#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

graph_avg_time <- df %>%
  group_by(user_id) %>%
  summarise(avg_days = mean(days_since_prior_order))
  
  ggplot(data = graph_avg_time, aes(x = avg_days, )) +
    geom_bar(fill = 'grey', colour = 'black') + 
    scale_x_continuous(breaks = 0:30) +
    labs(x = 'Average Returning Time', y = 'Number of Buyes', title = 'Graph 14')

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

ggplot(data = df, aes(x = days_since_prior_order)) +
  geom_bar(fill = 'grey', colour = 'black') +
  scale_x_continuous(breaks = 0:30) +
  labs(x = 'Days Since Prior Order', y = 'Number of Buyes', title = 'Graph 15')

    # Sim, há similaridade entre os gráficos, que pode indicar um comportamento contiuno entre os compradores. 

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

graph_avg_time_filter <- df %>%
  group_by(user_id) %>%
  summarise(orders  = n()) %>%
  filter(orders >= 5) %>%
  inner_join(df, by = 'user_id') %>%
  group_by(user_id) %>%
  summarise(avg_days = mean(days_since_prior_order)) %>%
  group_by(avg_days) %>%
  ungroup()

  ggplot(data = graph_avg_time, aes(x = avg_days, )) +
    geom_bar(fill = 'grey', colour = 'black') + 
    scale_x_continuous(breaks = 0:30) +
    labs(x = 'Average Returning Time', y = 'Number of Buyes', title = 'Graph 16')
  
    #Sim, o padrão se mantem.


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
  
bananas <- c(24852, 13176, 39276, 37067, 29259)

dataset_bananas <- insta_products %>%
  filter(product_id %in% bananas) %>%
  group_by(order_id) %>%
  summarise(quantidade = n()) %>%
  filter(quantidade > 1)

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

vetor_bananas <- insta_products %>%
  inner_join(dataset_bananas, by = 'order_id') %>%
  group_by(product_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(3)

vetor_3_bananas = as_vector(vetor_bananas$product_id)

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

orders_bananas <- insta_orders %>%
  inner_join(insta_products, by = 'order_id') %>%
  filter(product_id %in% vetor_3_bananas) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(quantidade = n_distinct(order_id)) %>%
  ungroup()

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora

  ggplot(data = orders_bananas, aes(x = 'order_dow', y = 'order_hour_of_day', size = 'quantidade')) +
    geom_point() +
    labs(x = 'Day of the Week', y = 'Time', title = 'Graph 20')

#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

