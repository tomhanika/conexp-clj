The aim of this page is to present and discuss commonly used file formats in FCA software. It may also serve as a platform to design new formats not yet implemented in any program.

## Burmeister Format

This file format is the file format of Burmeisters ConImp, and is supported by a number of programs. It is easily recognizable, human readable and small.

Burmeister's format is supported by ConImp, ConExp, conexp-clj.

### Structure

The file format is structured as follows:

- the first line consists of a single "B"
- the second line contains the name of the context (note that some programs ignore this)
- the third and fourth line consist of the object and attribute count, respectively
- the fifth line is empty or contains a comment
- after that, all objects and all attributes are listed, each on a separate line
- finally, the context is given as a combination of . and X, each row on a separate line. 

There are no restrictions on the context.

### Example

    B
    name
    2
    2

    a
    b
    1
    2
    .X
    XX

corresponds to the context

       | 1  2
    ---+------
     a | .  X 
     b | X  X

## Colibri

### Example

    a: 1
    b: 1,2

## ConExp

### Example

    <?xml version="1.0" encoding="UTF-8"?>
    <ConceptualSystem>
      <Version MajorNumber="1" MinorNumber="0"/>
      <Contexts>
        <Context Type="Binary" Identifier="0">
          <Attributes>
            <Attribute Identifier="1">
              <Name>2</Name>
            </Attribute>
            <Attribute Identifier="0">
              <Name>1</Name>
            </Attribute>
          </Attributes>
          <Objects>
            <Object>
              <Name>a</Name>
              <Intent>
                <HasAttribute AttributeIdentifier="1"/>
              </Intent>
            </Object>
            <Object>
              <Name>b</Name>
              <Intent>
                 <HasAttribute AttributeIdentifier="0"/>
                 <HasAttribute AttributeIdentifier="1"/>
              </Intent>
            </Object>
          </Objects>
        </Context>
      </Contexts>
    </ConceptualSystem>

corresponds to the context

       | 1 2 
    ---+-----
     a | . x
     b | x x

## CSV

### Example

    a,2
    b,1
    b,2

## FIMI

Galicia
Galicia2

### Example

    <Galicia_Document>
      <BinaryContext numberObj="2" numberAtt="2">
        <Name>Context</Name>
        <Object>a</Object>
        <Object>b</Object>
        <Attribute>1</Attribute>
        <Attribute>2</Attribute>
        <BinRel idxO="0" idxA="1"></BinRel>
        <BinRel idxO="1" idxA="0"></BinRel>
        <BinRel idxO="1" idxA="1"></BinRel>
      </BinaryContext>
    </Galicia_Document>

## Anonymous Burmeister

This file format does not really exist, it's only some idea to have an unrestricted anonymous format for formal contexts.

### Example

    A
    .X
    XX

## FCALGS

### Example

    2
    1 2
