% motor_inferencia.pl

:- consult('base_conhecimento.pl').

% declara que o predicado 'resposta' é dinâmico.
% isso permite que fatos sejam adicionados e removidos em tempo de execução.
:- dynamic resposta/1.

% predicado para calcular a pontuação de uma única trilha.
% encontra todas as características de uma trilha que o usuário marcou como 'sim'
% e soma os pesos  para obter a pontuação total.
calcular_pontuacao(Trilha, PontuacaoTotal) :-
    trilha(Trilha, _),
    findall(Peso, (perfil(Trilha, Caracteristica, Peso), resposta(Caracteristica)), Pesos),
    sum_list(Pesos, PontuacaoTotal).

% predicado para calcular as pontuações de todas as trilhas e gerar o ranking.
% utiliza findall/3 para coletar todas as pontuações e, em seguida,
% sort/4 para ordenar os resultados de forma decrescente.
calcular_e_exibir_ranking :-
    findall(Pontuacao-Trilha, calcular_pontuacao(Trilha, Pontuacao), ListaResultados),
    sort(0, @>=, ListaResultados, RankingOrdenado),
    exibir_ranking(RankingOrdenado).

% predicado para exibir o ranking final.
exibir_ranking([]) :-
    format('~nNenhuma trilha encontrada. Tente novamente respondendo "s" para algumas perguntas.~n', []).
exibir_ranking(Ranking) :-
    format('~n~s~n~s~n', ['---', 'Resultados da Recomendação']),
    exibir_item_ranking(Ranking).

% predicado para exibir cada item do ranking recursivamente.
exibir_item_ranking([]) :- !.
exibir_item_ranking([Pontuacao-Trilha|Resto]) :-
    trilha(Trilha, Descricao),
    format('~nTrilha: ~w~n', [Trilha]),
    format('Pontuação de compatibilidade: ~w~n', [Pontuacao]),
    format('Descrição: ~w~n', [Descricao]),
    exibir_item_ranking(Resto).

% predicado para executar testes com perfis em arquivos.
% limpa as respostas anteriores, carrega o arquivo de teste e exibe o ranking.
testar(ArquivoTeste) :-
    retractall(resposta(_)), % limpa respostas de execuções anteriores
    format('~n--- Testando com o perfil do arquivo: ~w ---~n', [ArquivoTeste]),
    consult(ArquivoTeste), % Carrega o arquivo de teste, adicionando os fatos de 'resposta'
    calcular_e_exibir_ranking.