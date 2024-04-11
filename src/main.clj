(ns main
  (:require [entidades] ))

;(doseq [professor @entidades/professores] (println professor))
(println "")

;(doseq [disciplina @entidades/disciplinas] (println disciplina))
(println "")

;(doseq [turma @entidades/turmas] (println turma))
(println "")

;clj -X core/run
(defn criar-matriz [colunas linhas]
  (vec (repeat colunas (vec (repeat linhas 0)))))

(defn cria-horario [turmas colunas linhas]
  (vec (repeat turmas (criar-matriz colunas linhas))))

(def minha-matriz-horario (cria-horario 3 5 2))

(defn dia-periodo []
  (for [dia (range (count (minha-matriz-horario 0)))
        periodo (range (count ( (minha-matriz-horario 0)0)))]
    [dia periodo]))

(println (dia-periodo) "dia periodo")

(defn turma-dia-periodo []
  (for [turma (range (count minha-matriz-horario))
        periodo (range (count (minha-matriz-horario 0)))
        dia (range (count ( (minha-matriz-horario 0)0)))
        ]
    [turma periodo dia]))

(println (vec(turma-dia-periodo)) "turma dia periodo")

(println minha-matriz-horario "minha matriz horario")


; Define a função condicao-de-parada para verificar se o estado atual é uma solução válida

(defn condicao-de-parada [disciplinas]
  (if (empty? disciplinas)
    true
    false)
  )

; Define a função atualizar-estado para atualizar o estado com uma nova possibilidade
(defn encontrar-indice-turma [turmas id-disciplina]
  (loop [indice 0
         turmas-restantes turmas]
    (if (empty? turmas-restantes)
      nil
      (let [turma (first turmas-restantes)
            disciplinas-turma (:disciplinas turma)]
        (if (some #(= id-disciplina %) disciplinas-turma)
          indice
          (recur (inc indice) (rest turmas-restantes)))))))

(defn encontrar-indice-professor [professores id-disciplina]
  (let [professor-encontrado (first (filter (fn [professor] (some #(= id-disciplina %) (:disciplinas professor))) professores))]
    (when professor-encontrado
      (.indexOf professores professor-encontrado))))

(defn encontrar-professor [professores id-disciplina]
  (first (filter (fn [professor] (some #(= id-disciplina %) (:disciplinas professor))) professores)))

(defn remover-horario [professor horario]
  (update-in professor [:disponibilidade]
             (fn [disponibilidade]
               (vec(remove #(= % horario) disponibilidade)))))


(defn atualizar-estado [estado disciplina professores turmas possibilidade]
  ;não inserir matéria em turma não correspondente
  ;Transformar em funções
    ;verifica atravez do id disciplina se a matéria esta sendo inserida (atravez da possibilidade) na turma certa
    ;verifica se professor possui aquela disponibilidade(passando possibilidade)
  (if (and (= (encontrar-indice-turma turmas (:id disciplina)) (first possibilidade)) (some #(= % (vec (rest possibilidade)) ) (:disponibilidade (encontrar-professor professores (:id disciplina)))))  ;se 0 , ensirir na turma 0 , se 1 , inserir na turma 1 , se 2 , inserir na turma 2
    {:estado (assoc-in estado possibilidade disciplina)
     :professores (assoc professores (encontrar-indice-professor professores (:id disciplina)) (remover-horario (professores(encontrar-indice-professor professores (:id disciplina))) (vec(rest possibilidade))))} ;retornar novo estado e lista de professores , tudo em um mapa
    nil) ;(encontrar-indice-professor professores (:id disciplina))
  ;usar let para não usar novamente a mesma função
  ;verificar para não ter mais de uma ou duas aulas de uma matéria em um dia.
  )

; Define as possibilidades a serem testadas
(def possibilidades (vec(turma-dia-periodo)))


(defn testar-possibilidades [estado disciplinas professores turmas possibilidades tentadas]
  (if (condicao-de-parada disciplinas)
    estado
    (loop [possibilidades possibilidades
           tentadas tentadas]
      ;(println (apply conj tentadas [(first possibilidades)]) "tentadas")
      (if (empty? possibilidades)
        nil
        (if-let [novo-estado (atualizar-estado estado (first disciplinas) professores turmas (first possibilidades))] ;if let ;se sim , matéria inserida , proximo loop com todas as possibilidades+tentadas porem menos a que ja foi validada. ;se não , menos uma possibilidade e mais uma tentadas.
          (if-let [solucao (testar-possibilidades (:estado novo-estado) (vec (rest disciplinas)) (:professores novo-estado) turmas (apply conj tentadas (rest possibilidades)) [] )]
            solucao
            (recur (vec (rest possibilidades)) (apply conj tentadas [(first possibilidades)]) )) ;(conj tentadas (first possibilidades))
          (recur (vec (rest possibilidades)) (apply conj tentadas [(first possibilidades)]) )))))) ;(conj tentadas (first possibilidades))

(defn expandir-disciplinas [disciplinas]
  (apply concat (for [disciplina disciplinas
                      :let [num-aulas-semana (:aula-semana disciplina)]
                      _ (range num-aulas-semana)]
                  [disciplina])))

;; Exemplo de uso
(defn print-disciplinas [disciplinas]
  (doseq [disciplina disciplinas]
    (let [id (:id disciplina)
          nome (:nome disciplina)]
      (print (format "ID: %2d, Nome: %-30s" id nome))
      )))


(defn visualizar-matriz [matriz]
  (doseq [coluna matriz]
    (doseq [disciplina coluna]
      (print-disciplinas disciplina)
      (println))
    (println)))



(def horario-final (testar-possibilidades minha-matriz-horario (expandir-disciplinas @entidades/disciplinas) @entidades/professores @entidades/turmas possibilidades [] ))

(visualizar-matriz horario-final)

(defn -main
  [& args]
  ;(println (testar-possibilidades minha-matriz-horario (expandir-disciplinas @entidades/disciplinas) @entidades/professores @entidades/turmas possibilidades [] ))
  ;(visualizar-matriz (testar-possibilidades minha-matriz-horario (expandir-disciplinas @entidades/disciplinas) @entidades/professores @entidades/turmas possibilidades [] ))

  ) ;(vec(range 30)) disciplinas

;ID:  1, Nome: Ingles                        ID:  2, Nome: Metodologia cientifica            ID:  3, Nome: Introdução a lógica                     ID:  4, Nome: Portugues                ID:  5, Nome: Introdução a computação
;ID:  1, Nome: Ingles                        ID:  2, Nome: Metodologia cientifica            ID:  3, Nome: Introdução a lógica                     ID:  4, Nome: Portugues                ID:  5, Nome: Introdução a computação

;ID:  6, Nome: Estrutura de dados         ID:  7, Nome: Sistemas operacionais             ID:  8, Nome: Banco de dados 1                        ID:  9, Nome: Redes                           ID: 10, Nome: Laboratorio de metodos cientificos
;ID:  6, Nome: Estrutura de dados         ID:  7, Nome: Sistemas operacionais             ID:  8, Nome: Banco de dados 1                        ID:  9, Nome: Redes                           ID: 11, Nome: Novas tecnologias

;ID: 12, Nome: Auditoria            ID: 13, Nome: Padrões de projeto        ID: 14, Nome: Engenharia de software                ID: 15, Nome: Emprededorismo                ID: 16, Nome: Desenvolvimento mobile
;ID: 12, Nome: Auditoria            ID: 13, Nome: Padrões de projeto        ID: 14, Nome: Engenharia de software                ID: 16, Nome: Desenvolvimento mobile        ID: 17, Nome: Projeto de pesquisa 1
