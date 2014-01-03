Divisione a due cifre
=====================

What is it?
-----------
Command line tool that computes double digit division.

Aiding my daughter with double digit division (elementary school assignments),
I found out its algorithmic nature. So I thought that this was a nice
way to try some Haskell.
As the method used in this program is specific to Italian elementary school
(and even there is taught with some variations), I decided to write the
documentation and the program itself in Italian.

Cos'è?
------
Programma a linea di comando che calcola la divisione a due cifre.

Stavo aiutando mia figlia con i compiti di matematica e ho scoperto
la natura algoritmica di questa divisione. Ho pensato che fosse un bel
modo per provare un po' di Haskell.

Il metodo usato è illustrato nel seguente diagramma di attività.

![Diagramma di attività della divisione a due cifre](divisione_due_cifre.png?raw=true)

Avvio veloce
------------
Esegui la divisione a due cifre senza argomenti e mostrerà l'aiuto:

    user@host:~$ ddc

    Usage: ddc DIVIDENDO DIVISORE
      Calcola il risultato delle divisioni a due cifre

    Available options:
      -h,--help                Mostra questo aiuto
      -v,--versione            Mostra informazioni sulla versione

Il programma si utilizza fornendo il dividendo ed il divisore come il primo
ed il secondo parametro.

        user@host:~$ cdc 1234 56

Licenza
-------
Verifica il file LICENSE.

Contatti
--------
Per domande e commenti:

- [MauroTaraborelli@gmail.com](mailto:MauroTaraborelli@gmail.com)
