##
## EPITECH PROJECT, 2021
## B-FUN-501-PAR-5-1-HAL-ilia.mishchenko
## File description:
## Makefile
##

STACKFLAGS	=	--system-ghc

MAKEFLAGS	=	-j

NAME		=	hal

all:
				stack build
				stack --local-bin-path . install
				mv $(NAME)-exe $(NAME)

clean:
				stack clean

fclean:			clean
				rm ../$(NAME)

re: 			fclean all

tests_run:
				stack test

.PHONY:			all clean fclean re
