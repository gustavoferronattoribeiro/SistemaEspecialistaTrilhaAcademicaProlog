% fatos: trilha(Nome, Descrição)
trilha(ciencia_de_dados, 'Análise e interpretação de grandes volumes de dados.').
trilha(deep_learning, 'Criação de redes neurais para resolver problemas complexos.').
trilha(desenvolvimento_web, 'Construção e manutenção de sites e aplicativos web.').
trilha(inteligencia_artificial, 'Criação de sistemas que podem aprender e tomar decisões.').
trilha(redes_e_infraestrutura, 'Gerenciamento de redes de computadores e infraestrutura de TI.').
trilha(seguranca_da_informacao, 'Proteção de sistemas, redes e dados contra ameaças.').
trilha(visao_computacional, 'Desenvolvimento de sistemas que "veem" e interpretam imagens.').

% fatos: perfil(Trilha, Característica, Peso)
perfil(ciencia_de_dados, matematica_estatistica, 5).
perfil(ciencia_de_dados, analise_critica, 5).
perfil(ciencia_de_dados, curiosidade, 4).

perfil(deep_learning, matematica_estatistica, 5).
perfil(deep_learning, logica_computacional, 5).
perfil(deep_learning, pesquisa_cientifica, 4).

perfil(desenvolvimento_web, logica_computacional, 4).
perfil(desenvolvimento_web, design_visual, 3).
perfil(desenvolvimento_web, trabalho_em_equipe, 4).

perfil(inteligencia_artificial, matematica_estatistica, 5).
perfil(inteligencia_artificial, logica_computacional, 4).
perfil(inteligencia_artificial, resolucao_de_problemas, 4).

perfil(redes_e_infraestrutura, resolucao_de_problemas, 4).
perfil(redes_e_infraestrutura, atencao_a_detalhes, 4).
perfil(redes_e_infraestrutura, trabalho_em_equipe, 3).

perfil(seguranca_da_informacao, resolucao_de_problemas, 5).
perfil(seguranca_da_informacao, curiosidade, 4).
perfil(seguranca_da_informacao, atencao_a_detalhes, 5).

perfil(visao_computacional, matematica_estatistica, 4).
perfil(visao_computacional, logica_computacional, 5).
perfil(visao_computacional, resolucao_de_problemas, 4).

% fatos: pergunta(ID, Texto, Característica)
pergunta(1, 'Você se interessa por como as coisas funcionam?', curiosidade).
pergunta(2, 'Você tem afinidade com matemática e estatística?', matematica_estatistica).
pergunta(3, 'Você gosta de resolver quebra-cabeças e problemas complexos?', resolucao_de_problemas).
pergunta(4, 'Você tem um olhar apurado para detalhes?', atencao_a_detalhes).
pergunta(5, 'Você gosta de trabalhar em equipe?', trabalho_em_equipe).
pergunta(6, 'Você tem um bom senso estético ou interesse em design?', design_visual).
pergunta(7, 'Você se interessa por lógica e raciocínio abstrato?', logica_computacional).
pergunta(8, 'Você gosta de ler artigos científicos e fazer pesquisas?', pesquisa_cientifica).
pergunta(9, 'Você é bom em analisar informações e tirar conclusões?', analise_critica).

:- dynamic resposta/1.

% predicado principal pra iniciar o programa
iniciar :-
    retractall(resposta(_)), % limpa respostas anteriores
    format('~n~s~n~s~n', ['Bem-vindo(a) ao sistema de recomendação de trilhas!', 'Responda com "s" (sim) ou "n" (não) para cada pergunta.']),

    % inicia o questionario, começando pela pergunta 1
    fazer_perguntas(1),

    % calcula e exibe o ranking final
    calcular_e_exibir_ranking.

% predicado pra perguntar de forma recursiva
fazer_perguntas(ID) :-
    % Condição de parada: se não houver mais perguntas, a recursão termina.
    \+ pergunta(ID, _, _),
    !.

fazer_perguntas(ID) :-
    % recursao
    pergunta(ID, Texto, Caracteristica),
    format('~n~w (s/n)? ', [Texto]),
    read_line_to_string(user_input, RespostaStr),
    ( (RespostaStr = "s"; RespostaStr = "sim") ->
        assertz(resposta(Caracteristica))
    ;
        true
    ),
    ProxID is ID + 1,
    fazer_perguntas(ProxID).

% predicado pra calcular a pontuação de uma unica trilha
calcular_pontuacao(Trilha, PontuacaoTotal) :-
    trilha(Trilha, _),
    findall(Peso, (perfil(Trilha, Caracteristica, Peso), resposta(Caracteristica)), Pesos),
    sum_list(Pesos, PontuacaoTotal).

% predicado pra calcular as pontuações de todas as trilhas e gerar o ranking
calcular_e_exibir_ranking :-

    % encontra todas as trilhas com suas pontuações e coloca em uma lista de pares (Pontuacao-Trilha)
    findall(Pontuacao-Trilha, calcular_pontuacao(Trilha, Pontuacao), ListaResultados),

    % remove duplicatas e ordena a lista em ordem decrescente de pontuação
    sort(0, @>=, ListaResultados, RankingOrdenado),

    % resultados
    exibir_ranking(RankingOrdenado).

% predicado pra exibir o ranking final
exibir_ranking([]) :-
    format('~nNenhuma trilha encontrada. Tente novamente respondendo "s" para algumas perguntas.~n', []).
exibir_ranking(Ranking) :-
    format('~n~s~n~s~n', ['---', 'Resultados da Recomendação']),
    exibir_item_ranking(Ranking).

% predicado pra exibir cada item do ranking
exibir_item_ranking([]) :- !.
exibir_item_ranking([Pontuacao-Trilha|Resto]) :-
    trilha(Trilha, Descricao),
    format('~nTrilha: ~w~n', [Trilha]),
    format('Pontuação de compatibilidade: ~w~n', [Pontuacao]),
    format('Descrição: ~w~n', [Descricao]),
    exibir_item_ranking(Resto).

% predicado pra executar testes com perfis em arquivos.
testar(ArquivoTeste) :-
    retractall(resposta(_)), % limpa respostas de execuções anteriores
    format('~n--- Testando com o perfil do arquivo: ~w ---~n', [ArquivoTeste]),
    consult(ArquivoTeste), % Carrega o arquivo de teste, adicionando os fatos de 'resposta'
    calcular_e_exibir_ranking.