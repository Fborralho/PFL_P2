# 2º Trabalho prático PFL
# Ano Letivo 2024/2025

## Introdução

Para este trabalho prático, desenvolvemos uma versão do jogo Storm Clouds em Prolog. O desenvolvimento do jogo foi realizado em várias etapas, entre as quais: criação da lógica do gameState interno, display do tabuleiro de jogo, leitura de inputs de jogadas de ambos os jogadores, validação de jogadas e atualização do gameState de acordo com as mesmas, criação do gameloop, criação do menu de jogo e expansão do modo de jogo humano vs humano, para os modos humano vs bot e bot vs bot.

A participação geral dos membros do grupo StormClouds_5 no trabalho foi a seguinte:

Francisco Borralho - 65% - criação da lógica do gameState interno, display do tabuleiro de jogo, leitura de inputs de jogadas de ambos os jogadores e criação do gameloop, validação de jogadas e atualização do gameState de acordo com as mesmas e criação dos menu de jogo 

Francisco Magalhães - 35% - display do tabuleiro de jogo, leitura de inputs de jogadas de ambos os jogadores e validação de jogadas e atualização do gameState de acordo com as mesmas.

## Execução do jogo

Para executar o jogo é apenas necessário, abrir o terminal do Sicstus após a sua instalação, compilar o ficheiro game.pl e correr o predicado play_game/0.

## Descrição do jogo

O jogo desenvolvido, Storm Clouds, consiste num jogo de tabuleiro 8x8 de captura de peças semelhante a damas ou xadrez, mas onde o objetivo é capturar todas as peças do adversário. Também, ao contrário do que acontece, por exemplo, no xadrez todas as peças de cada jogador se movimentam exatamente da mesma forma, tendo assim exatamente o mesmo valor no jogo.

Contudo, o que torna este jogo algo único é a disposição das peças no seu tablueiro de jogo. A distribuição é assimétrica, estando as 12 peças pretas, numa fase inicial, dispostas nas duas primeiras colunas do tabuleiro e as 12 brancas nas duas linhas inferiores da tabuleiro começando essas linhas a partir do lado direito do tabuleiro.

As peças pretas podem realizar movimentos de não captura de outras peças para norte (para cima na vertical), nordeste (em diagonal para cima e para a direita), este( direita no tabuleiro) e para sudeste (em diagonal para baixo e para a direita), enquanto as peças brancas podem realizar este tipo de movimentos para este, norte, nordeste e noroeste( em diagonal para cima e para a esquerda).

Ambas as peças podem realizar movimentos de captura em qualquer diagonal.

Uma descrição mais detalhada do jogo e das suas regras pode ser consultada em https://boardgamegeek.com/boardgame/429340/storm-clouds.

## Lógica de desenvolvimento do jogo

### Representação da configuração de jogo

A configuração do jogo contém as informações necessárias para definir as regras, o tabuleiro inicial, e os jogadores. A configuração é representada internamente através de predicados que estabelecem:

    Tabuleiro Inicial: Representado como uma lista de listas (matriz), onde cada célula contém um átomo indicando o estado (b para peça preta, w para peça branca e empty para células vazias).

    Jogadores: São representados pelos átomos black e white.

    Peças Capturadas: A contagem de peças capturadas por cada jogador é representada pelo predicado captured_pieces/2.

O predicado init_game_state/2 é responsável por configurar o estado inicial do jogo, garantindo que o tabuleiro esteja na configuração inicial e que todas as variáveis (jogador atual, peças capturadas e configurações) estejam corretamente inicializadas.

### Representação do estado interno do jogo

O estado interno do jogo é representado por uma estrutura composta chamada gameState(Board, CurrentPlayer, CapturedPieces, Config), onde:

    Board: Representa o tabuleiro como uma matriz de 8x8, onde:

        b: Peça do jogador preto.

        w: Peça do jogador branco.

        empty: Casa vazia.

    CurrentPlayer: Indica o jogador atual (black ou white).

    CapturedPieces: Representa as peças capturadas usando o predicado captured_pieces(BlackCount, WhiteCount), onde BlackCount é o número de peças capturadas pelo jogador preto e WhiteCount é o número de peças capturadas pelo jogador branco.
    

### Representação de movimentos

Uma jogada é representada por um tuple (FromRow, FromCol, ToRow, ToCol), que indica as coordenadas de origem e destino da peça. Essas coordenadas são números inteiros que correspondem às posições no tabuleiro. A jogada é validada e executada pelo predicado move/3, que:

- Verifica se a jogada é válida para o jogador atual, seja uma jogada de captura ou não.
- Verifica se a célula de origem contém uma peça do jogador atual.
- Certifica-se de que o destino está dentro do tabuleiro e que o caminho está livre.
- Atualiza o tabuleiro removendo a peça da posição inicial e colocando-a na posição final.
- Se for uma jogada de captura, atualiza a contagem de peças capturadas.
- Alterna para o próximo jogador.

### Interação com o jogador

#### Menu

O menu principal oferece três opções: "Como Jogar", "Jogar" e "Sair".

No modo "Jogar", o usuário pode escolher entre Player vs Player ou Player vs Computer, além de selecionar a sua cor em jogos contra o computador.

#### Registo de jogadas

As jogadas são inseridas com o formato (FromRow, FromCol, ToRow, ToCol), onde FromRow e FromCol representam, respetivamnte, a coluna e a linha onde a peça a ser movimentada se localiza no atual gameState e ToRow e ToCol representam, respetivamente, coluna e a linha para as quais o jogador deseja movimentar a peça.

- É verificado se a jogada é válida antes de ser processada.
- Caso seja inválida, o jogo informa o jogador e solicita uma nova jogada.
- O jogador pode, em qualquer das suas jogadas, escrever stop para encerrar o jogo.

#### Feedback ao jogador

O sistema exibe mensagens indicando o turno atual, o estado do tabuleiro, e a pontuação de cada jogador.

Após cada jogada, informações sobre o estado do jogo (capturas, jogador vencedor, etc.) são atualizadas e exibidas.

### Conclusões

#### Pontos positivos

A lógica do jogo foi implementada para lidar com aspectos essenciais, como validação de movimentos, capturas, alternância de turnos e condições de vitória.

Movimentos de captura e não captura são validados por predicados claros e modulares, garantindo uma jogabilidade precisa e eficiente.

O menu permite que os jogadores escolham modos de jogo, vejam instruções e comecem ou encerrem o jogo de forma simples.

A validação de jogadas garante que os movimentos fornecidos pelos jogadores sigam as regras, solicitando novas jogadas em caso de erro.

O estado do jogo e a configuração do tabuleiro são representados por estruturas intuitivas do Prolog, facilitando a leitura e futuras extensões do código.

#### Pontos negativos/Aspetos a melhorar

Infelizmente, por falta de tempo, não foi possível implementar corretamente os modo humano vs bot ou bot vs bot, apenas tedno o modo humano vs humano funcional.

A interface visual do jogo, nomeadamente, o display do tabuleiro está algo simplificada e pouco apelativa. 

