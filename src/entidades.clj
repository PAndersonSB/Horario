(ns entidades)

(defrecord Professor [id nome disciplinas disponibilidade]);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Disciplina [id nome aula-semana aula-dia]);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Turma [id nome disciplinas]) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Criando uma lista vazia
(def professores (atom '[]))
(def disciplinas (atom '[]))
(def turmas      (atom '[]))

(defn criar-professor
  ([id nome disciplinas] (->Professor id nome disciplinas nil))
  ([id nome disciplinas disponibilidade] (->Professor id nome disciplinas disponibilidade)))

(swap! professores conj (criar-professor 1 "Diandra" [1] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 2 "Laura" [2] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 3 "Vinicius" [3 8 17] [[2 0] [2 1] [3 0] [3 1] [4 1]]))
(swap! professores conj (criar-professor 4 "Marlecia" [3] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 5 "Diocles" [4] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 6 "Julio Oliveira" [5 10 12] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))

(swap! professores conj (criar-professor 7 "Fabio Oliveira" [6 16] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 8 "Marcos Paranhos" [7] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 9 "Jurandir" [9] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 10 "Arthur Dantas" [11 13 14] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))
(swap! professores conj (criar-professor 11 "Nadia" [15] [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1] [4 0] [4 1]]))

(swap! disciplinas conj (->Disciplina 1 "Ingles" 2 1))
(swap! disciplinas conj (->Disciplina 2 "Metodologia cientifica" 2 1))
(swap! disciplinas conj (->Disciplina 3 "Introdução a lógica" 2 1))
(swap! disciplinas conj (->Disciplina 4 "Portugues" 2 1))
(swap! disciplinas conj (->Disciplina 5 "Introdução a computação" 2 1))

(swap! disciplinas conj (->Disciplina 6 "Estrutura de dados" 2 1))
(swap! disciplinas conj (->Disciplina 7 "Sistemas operacionais" 2 1))
(swap! disciplinas conj (->Disciplina 8 "Banco de dados 1" 2 1))
(swap! disciplinas conj (->Disciplina 9 "Redes" 2 1))
(swap! disciplinas conj (->Disciplina 10 "Laboratorio de metodos cientificos" 1 1))
(swap! disciplinas conj (->Disciplina 11 "Novas tecnologias" 1 1))

(swap! disciplinas conj (->Disciplina 12 "Auditoria" 2 1))
(swap! disciplinas conj (->Disciplina 13 "Padrões de projeto" 2 1))
(swap! disciplinas conj (->Disciplina 14 "Engenharia de software" 2 1))
(swap! disciplinas conj (->Disciplina 15 "Emprededorismo" 1 1))
(swap! disciplinas conj (->Disciplina 16 "Desenvolvimento mobile" 2 1))
(swap! disciplinas conj (->Disciplina 17 "Projeto de pesquisa 1" 1 1))

(swap! turmas conj (->Turma 1 "primeiro semestre" [1 2 3 4 5]))
(swap! turmas conj (->Turma 2 "terceiro semestre" [6 7 8 9 10 11]))
(swap! turmas conj (->Turma 3 "quinto semestre"   [12 13 14 15 16 17]))

(defn encontrar-indice-professor [professores id-disciplina]
  (let [professor-encontrado (first (filter (fn [professor] (some #(= id-disciplina %) (:disciplinas professor))) professores))]
    (when professor-encontrado
      (.indexOf professores professor-encontrado))))

;; Exemplo de uso
(defn print-disciplinas [disciplinas]
  (doseq [disciplina disciplinas]
    (let [id (:id disciplina)
          nome (:nome disciplina)]
      (print (format "ID: %2d, Nome: %-30s" id nome))
      )))


(defn visualizar-matriz [matriz]
  (doseq [linha matriz]
    (doseq [disciplina linha]
      (print-disciplinas disciplina)
      (println))
    (println)))


(defn print-disciplinas [disciplinas]
  (let [output (apply str (for [disciplina disciplinas]
                            (let [id (:id disciplina)
                                  nome (:nome disciplina)]
                              (format "ID: %2d, Nome: %-30s" id nome))))]
    (print output)))


(defn visualizar-matriz [matriz]
  (doseq [linha matriz]
    (doseq [disciplina linha]
      (print-disciplinas disciplina)
      (print "  ")) ;; Adicionando um espaço entre as disciplinas
    (println)))