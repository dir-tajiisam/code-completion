import React, { useRef } from "react";
import {
  Button,
  ButtonProps,
  Drawer,
  DrawerBody,
  DrawerCloseButton,
  DrawerContent,
  DrawerFooter,
  DrawerHeader,
  DrawerOverlay,
  DrawerProps,
  Input,
  Radio,
  Stack,
  useDisclosure,
} from "@chakra-ui/react";
import { FC, useState } from "react";

const DrawerMenu: FC<ButtonProps> = (props: ButtonProps) => {
  const { isOpen, onOpen, onClose } = useDisclosure();
  const btnRef = useRef();

  return (
    <>
      <Button ref={btnRef} colorScheme="teal" onClick={onOpen} {...props}>
        Open
      </Button>
      <Drawer
        isOpen={isOpen}
        placement="right"
        onClose={onClose}
        // finalFocusRef={btnRef}
      >
        <DrawerOverlay />
        <DrawerContent>
          <DrawerCloseButton />
          <DrawerHeader>Create your account</DrawerHeader>

          <DrawerBody>
            <Stack>
              <Radio size="sm" name="1" colorScheme="red">
                Radio
              </Radio>
              <Radio size="md" name="1" colorScheme="green">
                Radio
              </Radio>
              <Radio size="lg" name="1" colorScheme="orange" defaultChecked>
                Radio
              </Radio>
            </Stack>
          </DrawerBody>

          <DrawerFooter>
            <Button variant="outline" mr={3} onClick={onClose}>
              Cancel
            </Button>
            <Button colorScheme="blue">Save</Button>
          </DrawerFooter>
        </DrawerContent>
      </Drawer>
    </>
  );
};

export default DrawerMenu;
