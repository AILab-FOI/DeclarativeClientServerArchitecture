import {
  Directive,
  HostListener,
  input,
  output,
  viewChild,
  ViewContainerRef,
} from '@angular/core';
import { ButtonComponent } from '../../shared/button/button.component';

@Directive({
  selector: '[appHoverable]',
  standalone: true,
})
export class HoverableDirective {
  public isHoverable = input.required<boolean>();
  public btn = viewChild('delete', { read: ViewContainerRef });
  public query = output<boolean>();

  @HostListener('mouseenter', ['$event'])
  mouseenter(): void {
    if (this.isHoverable()) {
      let ref = this.btn().createComponent(ButtonComponent);
      ref.setInput('title', 'aaaa');
      ref.instance.query.subscribe((res) => {
        this.query.emit(res);
      });
    }
  }

  @HostListener('mouseleave', ['$event'])
  mouseleave(): void {
    if (this.btn()) {
      this.btn().clear();
    }
  }
  constructor() {}
}
