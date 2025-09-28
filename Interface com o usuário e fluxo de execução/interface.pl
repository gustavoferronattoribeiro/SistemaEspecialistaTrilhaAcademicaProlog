% interface.pl

:- consult('motor_inferencia.pl').

% predicado principal para iniciar o programa.
% Limpa respostas anteriores, exibe uma mensagem de boas-vindas, inicia o questionário e,
% ao final, chama o motor de inferência para calcular e exibir o ranking.
iniciar :-
    retractall(resposta(_)), % limpa respostas anteriores
    format('~n~s~n~s~n', ['Bem-vindo(a) ao sistema de recomendação de trilhas!', 'Responda com "s" (sim) ou "n" (não) para cada pergunta.']),
    fazer_perguntas(1),
    calcular_e_exibir_ranking.

% predicado para perguntar de forma recursiva.
% o predicado encontra a pergunta pelo ID, a exibe para o usuário, lê a resposta e
% armazena a característica correspondente se a resposta for 'sim'.
fazer_perguntas(ID) :-
    \+ pergunta(ID, _, _), % Condição de parada: se não houver mais perguntas, a recursão termina.
    !.
fazer_perguntas(ID) :-
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