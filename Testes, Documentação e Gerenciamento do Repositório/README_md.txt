# Sistema Especialista para Recomendação de Trilha Acadêmica

## Descrição do Projeto

Este é um sistema especialista em Prolog projetado para recomendar uma trilha acadêmica na área de Tecnologia da Informação com base nos interesses e características do usuário. O sistema utiliza uma base de conhecimento para mapear as habilidades e características desejadas para cada trilha e um motor de inferência para calcular a pontuação de compatibilidade.

## Estrutura do Projeto

O projeto é dividido em módulos para facilitar a organização, manutenção e testes:

- `base_conhecimento.pl`: Contém todos os fatos (trilhas, perfis, perguntas) que formam a base de conhecimento.
- `motor_inferencia.pl`: Contém as regras e predicados para o processamento lógico, como cálculo de pontuações e ordenação.
- `interface.pl`: Gerencia a interação com o usuário, exibindo perguntas e coletando respostas.
- `testes/`: Pasta contendo arquivos para testes automatizados.

## Como Executar

1.  Certifique-se de ter um interpretador Prolog (como SWI-Prolog) instalado.
2.  Abra o interpretador Prolog.
3.  Carregue o arquivo `interface.pl` usando o comando `consult/1`:
    ```prolog
    ?- consult('interface.pl').
    ```
4.  Execute o predicado principal para iniciar o sistema:
    ```prolog
    ?- iniciar.
    ```
5.  Responda às perguntas com `s` para sim ou `n` para não.

## Como Executar os Testes

Para testar o sistema com perfis predefinidos, use o predicado `testar/1` disponível no `motor_inferencia.pl`.

1.  Carregue o arquivo `interface.pl` (ou `motor_inferencia.pl`).
2.  Execute o predicado `testar/1` com o nome do arquivo de teste desejado:
    ```prolog
    ?- testar('testes/perfil_seguranca.pl').
    
    ?- testar('testes/perfil_dev_web.pl').
    
    ?- testar('testes/perfil_analitico.pl').
    ```

---

Com essa divisão, seu projeto fica muito mais profissional e organizado. Cada arquivo tem uma responsabilidade clara, o que facilita o entendimento, a depuração e futuras expansões.