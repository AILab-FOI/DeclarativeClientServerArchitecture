import {
  Directive,
  ElementRef,
  HostListener,
  signal,
  viewChild,
} from '@angular/core';

@Directive({
  selector: '[appOutsideClick]',
  standalone: true,
})
export class OutsideClickDirective {
  public show = signal<boolean>(false);
  private element = viewChild('element', { read: ElementRef });
  @HostListener('document:mousedown', ['$event'])
  outsideClick(event: MouseEvent): void {
    if (!this.element()?.nativeElement.contains(event.target)) {
      this.show.set(false);
    }
  }

  constructor() {}
}
