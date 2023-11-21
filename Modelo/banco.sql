-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema banco
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema banco
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `banco` ;
USE `banco` ;

-- -----------------------------------------------------
-- Table `banco`.`domicilio`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`domicilio` (
  `id_dom` INT UNSIGNED NOT NULL COMMENT 'Clave primaria',
  `calle_dom` VARCHAR(45) NOT NULL COMMENT 'Calle',
  `num_dom` VARCHAR(3) NULL COMMENT 'Número',
  `cod_post_dom` VARCHAR(5) NOT NULL COMMENT 'Código postal',
  `prov_dom` VARCHAR(45) NOT NULL COMMENT 'Provincia',
  `pobl_dom` VARCHAR(45) NOT NULL COMMENT 'Población',
  PRIMARY KEY (`id_dom`))
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades domicilio';


-- -----------------------------------------------------
-- Table `banco`.`cliente`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`cliente` (
  `id_cliente` INT UNSIGNED NOT NULL COMMENT 'Clave primaria',
  `nif_cliente` VARCHAR(10) NOT NULL COMMENT 'NIF',
  `nom_cliente` VARCHAR(45) NOT NULL COMMENT 'Nombre',
  `fec_nac_cliente` DATE NOT NULL COMMENT 'Fecha de nacimiento',
  `id_dom` INT NULL COMMENT 'Clave primaria del domicilio',
  PRIMARY KEY (`id_cliente`),
  INDEX `fk_cliente_domicilio_idx` (`id_dom` ASC) VISIBLE,
  UNIQUE INDEX `nif_cliente_UNIQUE` (`nif_cliente` ASC) VISIBLE,
  CONSTRAINT `fk_cliente_domicilio`
    FOREIGN KEY (`id_dom`)
    REFERENCES `banco`.`domicilio` (`id_dom`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades cliente';


-- -----------------------------------------------------
-- Table `banco`.`medio`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`medio` (
  `id_medio` INT UNSIGNED NOT NULL COMMENT 'Clave primaria del medio de pago',
  `tip_med` ENUM('CTA', 'TRJ') NOT NULL COMMENT 'Tipo de medio: cuenta o tarjeta',
  PRIMARY KEY (`id_medio`))
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades medios de pago';


-- -----------------------------------------------------
-- Table `banco`.`cuenta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`cuenta` (
  `id_medio` INT UNSIGNED NOT NULL COMMENT 'Clave primaria de la cuenta',
  `num_cuenta` VARCHAR(20) NOT NULL COMMENT 'Número',
  `saldo_cuenta` DECIMAL(8,2) NOT NULL COMMENT 'Saldo',
  PRIMARY KEY (`id_medio`),
  UNIQUE INDEX `num_cuenta_UNIQUE` (`num_cuenta` ASC) VISIBLE,
  INDEX `fk_cuenta_medio1_idx` (`id_medio` ASC) VISIBLE,
  CONSTRAINT `fk_cuenta_medio1`
    FOREIGN KEY (`id_medio`)
    REFERENCES `banco`.`medio` (`id_medio`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades cuenta';


-- -----------------------------------------------------
-- Table `banco`.`cliente_rln_cuenta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`cliente_rln_cuenta` (
  `id_cliente` INT UNSIGNED NOT NULL COMMENT 'Clave primaria del cliente',
  `id_medio` INT UNSIGNED NOT NULL COMMENT 'Clave primaria de la cuenta',
  `tip_rln` ENUM('TIT', 'COTIT', 'AUT') NOT NULL COMMENT 'Tipo de relación entre el cliente y la cuenta: titular, cotitular o autorizado',
  PRIMARY KEY (`id_cliente`, `id_medio`),
  INDEX `fk_cliente_rln_cuenta_cuenta1_idx` (`id_medio` ASC) VISIBLE,
  INDEX `fk_cliente_rln_cuenta_cliente1_idx` (`id_cliente` ASC) VISIBLE,
  CONSTRAINT `fk_cliente_rln_cuenta_cliente1`
    FOREIGN KEY (`id_cliente`)
    REFERENCES `banco`.`cliente` (`id_cliente`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_cliente_rln_cuenta_cuenta1`
    FOREIGN KEY (`id_medio`)
    REFERENCES `banco`.`cuenta` (`id_medio`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
COMMENT = 'Conjunto de relaciones entre cliente y cuenta';


-- -----------------------------------------------------
-- Table `banco`.`tarjeta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`tarjeta` (
  `id_medio` INT UNSIGNED NOT NULL COMMENT 'Clave primaria de la tarjeta',
  `id_cliente` INT UNSIGNED NOT NULL COMMENT 'Clave primaria del cliente',
  `id_medio_cta` INT UNSIGNED NOT NULL COMMENT 'Clave primaria de la cuenta',
  `num_tarjeta` VARCHAR(16) NOT NULL COMMENT 'Número',
  `ccv_tarjeta` VARCHAR(3) NOT NULL COMMENT 'CCV',
  `cred_tarjeta` DECIMAL(8,2) NOT NULL COMMENT 'Crédito de la tarjeta',
  `fec_tarjeta` DATE NOT NULL COMMENT 'Fecha de caducidad de la tarjeta',
  UNIQUE INDEX `num_tarjeta_UNIQUE` (`num_tarjeta` ASC) VISIBLE,
  PRIMARY KEY (`id_medio`),
  INDEX `fk_tarjeta_medio1_idx` (`id_medio` ASC) VISIBLE,
  UNIQUE INDEX `fk_tarjeta_cliente_rln_cuenta1_UNIQUE` (`id_cliente` ASC, `id_medio_cta` ASC) VISIBLE,
  CONSTRAINT `fk_tarjeta_medio1`
    FOREIGN KEY (`id_medio`)
    REFERENCES `banco`.`medio` (`id_medio`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_tarjeta_cliente_rln_cuenta1`
    FOREIGN KEY (`id_cliente` , `id_medio_cta`)
    REFERENCES `banco`.`cliente_rln_cuenta` (`id_cliente` , `id_medio`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades tarjeta de crédito';


-- -----------------------------------------------------
-- Table `banco`.`movimiento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `banco`.`movimiento` (
  `id_medio` INT UNSIGNED NOT NULL COMMENT 'Clave primaria del medio',
  `fec_mov` TIMESTAMP(6) NOT NULL COMMENT 'Fecha del movimiento. Es el discriminante de la relación identificativa con la entidad medio.',
  `cnpt_mov` VARCHAR(45) NOT NULL COMMENT 'Concepto del movimiento',
  `importe_mov` DECIMAL(8,2) NOT NULL COMMENT 'Importe del  movimiento',
  PRIMARY KEY (`id_medio`, `fec_mov`),
  CONSTRAINT `fk_movimiento_medio1`
    FOREIGN KEY (`id_medio`)
    REFERENCES `banco`.`medio` (`id_medio`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
COMMENT = 'Conjunto de entidades movimiento de tarjeta o cuenta';


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
-- begin attached script 'script'
-- Secuencias.
-- -----------
CREATE or REPLACE SEQUENCE banco.cliente_id_cliente_seq       INCREMENT BY 1 START WITH 1;
CREATE or REPLACE SEQUENCE banco.domicilio_id_dom_seq         INCREMENT BY 1 START WITH 1;
CREATE or REPLACE SEQUENCE banco.medio_id_medio_seq           INCREMENT BY 1 START WITH 1;
CREATE or REPLACE SEQUENCE banco.movimiento_id_movimiento_seq INCREMENT BY 1 START WITH 1;

-- end attached script 'script'
