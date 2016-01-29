C     Declaracoes
      real*8 rnum(20000),pmod,dmax
      real*8 tec(3000),ts(3000),treal(3000),tini(3000)
      real*8 tfim(3000),tfila(3000),tsis(3000),tcpu(3000)
      real*8 trest(3000),ratual,rprox,montreal,rtowrite
      integer iseed,is,numrand,numproc,nextI,prevI,sizef(3000)
      integer stseed, endseed

      open(1, file= 'table.csv',status = 'unknown')

C     Formatos
11    format('# Processo,T Entre Chegadas,T Servico,T Real,T Inicio,T Fi
     *m,T Fila,T Sistema,T CPU Ociosa,Size Fila,T Restante,Seed')
12    format(I5,',',F14.6,',',F14.6,',',F14.6,',',F14.6,',',F14.6,',',F1
     *4.6,',',F14.6,',',F14.6,',',I9,',',F14.6,',',I9)
C 13    format(I5,',',F15.10)
      
C     Variaveis Fixas
      is = 1
      numrand = 10000
      numproc = 1000
      stseed = 37
      endseed = 63

C     Inicializacao
      write(1,11)
      do i=1,3000
          tec(i) = 0
          ts(i) = 0
          treal(i) = 0
          tini(i) = 0
          tfim(i) = 0
          tfila(i) = 0
          tsis(i) = 0
          tcpu(i) = 0
          sizef(i) = 0
          trest(i) = 0
      end do
      do i=1,numrand
          rnum(i) = 0.0D0
      end do
      ratual = 0
      rprox = 0
      montreal = 0
      rtowrite = 0

      do is=stseed,endseed
C         Gerar Aleatorios
          iseed = is
          pmod = 2147483647.D0
          dmax = 1.0D0/pmod
          numrand = numrand + 1
          rnum(1) = iseed * dmax
          do i=2,numrand
             rnum(i) = cong16807(iseed)
          end do
          
C         Percorre todos, anotando o TEC, TS e treal inicial
C         Em seguida, vai
C         Em seguida, vai calcular os outros campos
C         Por fim, recalcula o minI
  
C         Calculo com intervalo normal
          do i=1,numproc
             ratual = rnum(i)
             rprox = rnum(i+numproc)

C            Calculo de TEC, TS, T Real e T Restante
             if (ratual <= 0.46) then
                tec(i) = 15
             else if (ratual <= 0.73) then
                tec(i) = 45
             else if (ratual <= 0.87) then
                tec(i) = 75
             else if (ratual <= 0.93) then
                tec(i) = 105
             else if (ratual <= 0.96) then
                tec(i) = 135
             else if (ratual <= 0.98) then
                tec(i) = 165
             else if (ratual <= 1.00) then
                tec(i) = 195
             end if

             if (rprox <= 0.60) then
                ts(i) = -log(rprox)*40.0D0
             else if (rprox <= 0.85) then
                ts(i) = -log(rprox)*150.0D0
             else if (rprox <= 1.00) then
                ts(i) = -log(rprox)*300.0D0
             end if

             if (i == 0) then
                treal(i) = tec(i)
             else
                 treal(i) = tec(i) + treal(i-1)
             end if
             
             trest(i) = ts(i)
          end do

C         Rodar outside para o primeiro run
          prevI = 1
          i = 1
          tini(i) = treal(i)
          rtowrite = treal(i)
          
C         Nao sera possivel terminar o processo
          if (trest(i) >= 50) then
                tfim(i) = tini(i) + 50
                tcpu(i) = 0
                trest(i) = trest(i) - 50
C               Entra na fila novamente
                treal(i) = tfim(i)
C         Processo sera encerrado
          else
                 tfim(i) = tini(i) + trest(i)
                 tcpu(i) = 50 - trest(i)
                 trest(i) = trest(i) - trest(i)
          end if
          tfila(i) = tini(i) - rtowrite
          tsis(i) = tfim(i) - rtowrite
          
          nextI = 0
          montreal = 999999999999999999999999.9d0
          do k=1,numproc
             if (trest(k) > 0) then
C               Calcular fila
                if ((tfim(i) + tcpu(i)) >= treal(k)) then
                   sizef(i) = sizef(i) + 1
                end if
C               Encontrar proximo
                if (montreal > treal(k)) then
                   montreal = treal(k)
                   nextI = k
                end if
             end if
          end do

C     Se escreve rtowrite pois e o valor previo do treal, se mudou------
      write(1,12)i,tec(i),ts(i),rtowrite,tini(i),tfim(i),tfila(i),tsis(i
     *),tcpu(i),sizef(i),trest(i),is

C         Se rtowrite e treal, processo entrou na fila     
          if (rtowrite /= treal(i)) then
             tec(i) = 0
             ts(i) = trest(i)
          end if

C         Inicio do loop principal
666       if (nextI /= 0) then
             i = nextI
             if (treal(i) >= (tfim(prevI) + tcpu(prevI))) then
                tini(i) = treal(i)
             else
                tini(i) = tfim(prevI) + tcpu(prevI)
             end if
             
             rtowrite = treal(i)

C            Nao sera possivel terminar o processo
             if (trest(i) >= 50) then
                tfim(i) = tini(i) + 50
                tcpu(i) = 0
                trest(i) = trest(i) - 50
C               Entra na fila novamente
                treal(i) = tfim(i)
C            Processo sera encerrado
             else
                 tfim(i) = tini(i) + trest(i)
                 tcpu(i) = 50 - trest(i)
                 trest(i) = 0
             end if
             tfila(i) = tini(i) - rtowrite
             tsis(i) = tfim(i) - rtowrite

             prevI = i
             nextI = 0
             montreal = 999999999999999999999999.9d0
             do k=1,numproc
                if (trest(k) > 0) then
C                  Calcular fila
                   if ((tfim(i) + tcpu(i)) >= treal(k)) then
                      sizef(i) = sizef(i) + 1
                   end if
C                  Encontrar proximo
                   if (montreal > treal(k)) then
                      montreal = treal(k)
                      nextI = k
                   end if
                end if
             end do

C            T S R I F Y C N L
C            X X         X X X

      write(1,12)i,tec(i),ts(i),rtowrite,tini(i),tfim(i),tfila(i),tsis(i
     *),tcpu(i),sizef(i),trest(i),is

C            Se rtowrite e treal, processo entrou na fila     
             if (rtowrite /= treal(i)) then
                tec(i) = 0
                ts(i) = trest(i)
C               Zerar fila
                sizef(i) = 0
             end if

             goto 666
          end if

C         Calcular a media das variaveis
C          r1 = stls/tfim(gerar)
C          r2 = stec/gerar
C          r3 = sts/gerar
C          r4 = swq/gerar
C          r5 = nwait/gerar
C          r6 = stsis/gerar

C         Limpar variaveis  
          do i=1,3000
              tec(i) = 0
              ts(i) = 0
              treal(i) = 0
              tini(i) = 0
              tfim(i) = 0
              tfila(i) = 0
              tsis(i) = 0
              tcpu(i) = 0
              sizef(i) = 0
              trest(i) = 0
          end do
          do i=1,numrand
              rnum(i) = 0.0D0
          end do
          ratual = 0
          rprox = 0
          rtowrite = 0
          montreal = 0
          rtowrite = 0
      end do

C     Fechar arquivos
      close(1)
C      close(2)
C      close(3)
C      close(4)
C      close(5)
C      close(6)
C      close(7)
C      close(8)


C             stls = stls + tls
C             stec = stec + tec(i)
C             sts = sts + ts(i)
C             swq = swq + wq
C             if (wq == 0.0D0) then
C                  nwait = nwait + 1.0D0
C             end if
C             stsis = stsis + tsis


      end

      function cong16807(iseed)
        integer iseed,imod
        real*8 rmod,pmod,dmax
        rmod = dfloat(iseed)
        pmod = 2147483647.0D0
        dmax = 1.0D0/pmod
        rmod = rmod*16807.D0
        imod = rmod * dmax
        rmod = rmod - pmod* imod
        cong16807 = rmod * dmax
        iseed = rmod
        return
      end



