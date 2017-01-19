/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *   the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2006 */

/* vermilionsands 2015 */

package clojure.lang;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class IconoclastReflector {

  static public Field getField(Class c, String name, boolean getStatics) {
    return getField(c, name, getStatics, false, false);
  }

  static public Field getField(Class c, String name, boolean getStatics,
      boolean includeProtected, boolean includePrivate) {
    Field[] allfields;
    if (!includeProtected && !includePrivate) {
      allfields = c.getFields();
    } else {
      allfields = c.getDeclaredFields();
    }
    for (int i = 0; i < allfields.length; i++) {
      if (name.equals(allfields[i].getName())
          && Modifier.isStatic(allfields[i].getModifiers()) == getStatics
          && !(Modifier.isProtected(allfields[i].getModifiers()) && !includeProtected)
          && !(Modifier.isPrivate(allfields[i].getModifiers()) && !includePrivate))
        return allfields[i];
    }

    if (c.getSuperclass() != null && includeProtected) {
      return getField(c.getSuperclass(), name, getStatics, true, false);
    }

    return null;
  }

  static public List getMethods(Class c, int arity, String name, boolean getStatics) {
    return getMethods(c, arity, name, getStatics, false, false);
  }

  static public List getMethods(Class c, int arity, String name, boolean getStatics,
      boolean includeProtected, boolean includePrivate) {
    Method[] allmethods = null;
    if (!includeProtected && !includePrivate) {
      allmethods =c.getMethods();
    } else {
      allmethods = c.getDeclaredMethods();
    }
    ArrayList methods = new ArrayList();
    ArrayList bridgeMethods = new ArrayList();
    for (int i = 0; i < allmethods.length; i++) {
      Method method = allmethods[i];
      if (name.equals(method.getName())
          && Modifier.isStatic(method.getModifiers()) == getStatics
          && method.getParameterTypes().length == arity
          && !(Modifier.isProtected(method.getModifiers()) && !includeProtected)
          && !(Modifier.isPrivate(method.getModifiers()) && !includePrivate)) {
        try {
          if (method.isBridge() && c.getMethod(method.getName(), method.getParameterTypes()).equals(method)) {
            bridgeMethods.add(method);
          } else {
            methods.add(method);
          }
        } catch (NoSuchMethodException e) {

        }
      }
    }

    if (methods.isEmpty()) {
      methods.addAll(bridgeMethods);
    }

    if (!getStatics && c.isInterface()) {
      allmethods = Object.class.getMethods();
      for (int i = 0; i < allmethods.length; i++) {
        if (name.equals(allmethods[i].getName())
            && Modifier.isStatic(allmethods[i].getModifiers()) == getStatics
            && allmethods[i].getParameterTypes().length == arity) {
          methods.add(allmethods[i]);
        }
      }
    }

    if (methods.isEmpty() && c.getSuperclass() != null && includeProtected) {
      methods = (ArrayList)getMethods(c.getSuperclass(), arity, name, getStatics, true, false);
    }

    return methods;
  }

  public static boolean isAncestorClass(Class ancestor, Class descendant) {
    if (descendant.equals(Object.class)) {
      return false;
    }

    Class superClass = descendant.getSuperclass();
    if (superClass.equals(ancestor)) {
      return true;
    }

    return isAncestorClass(ancestor, superClass);
  }

}
