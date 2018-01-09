
INCLUDE "memory.x"
INCLUDE "periph.x"

SECTIONS {
    .bss :  { *(.bss) } > ram
    .data : { *(.data) } > ram
    .text : { *(.text) } > rom
    .vectors : { *(.vectors) } > vectors
}

